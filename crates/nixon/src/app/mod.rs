//! The subcommand layer. SPEC §10, ENGINEERING §4.2.

pub mod edit;
pub mod eval;
pub mod gc;
pub mod handle;
pub mod new;
pub mod project;
pub mod run;

use std::path::{Path, PathBuf};

use nixon_picker::Picker;

use crate::command::Command;
use crate::config::{Config, load};
use crate::discover::find_project_commands;
use crate::error::{NixonError, Result};
use crate::eval::Context;
use crate::fs::Dirs;
use crate::process::ProcessRunner;
use crate::project::detect::find_in_project_or_default;
use crate::project::{Project, discover};

/// What the subcommands need from the environment, passed in rather than
/// read, so the whole layer runs in tests. ENGINEERING §4.2.
#[derive(Clone, Debug, Default)]
pub struct Environment {
    /// Where nixon was invoked.
    pub cwd: PathBuf,
    /// `$SHELL`, for commands with no language. SPEC §7.1.
    pub shell: Option<String>,
    /// `$DIRENV_DIR`, for the direnv wrapper. SPEC §7.3.
    pub direnv_dir: Option<String>,
    /// `$VISUAL`, then `$EDITOR`; `nano` when neither is set. SPEC §10.5.
    pub editor: Option<String>,
}

impl Environment {
    /// The editor to open a command in. SPEC §10.5.
    pub fn editor(&self) -> &str {
        self.editor.as_deref().unwrap_or("nano")
    }
}

/// Options shared by `run` and the command half of `project`. SPEC §10.1.
#[derive(Clone, Debug, Default)]
pub struct RunOpts {
    /// The command name, used as the picker's query.
    pub command: Option<String>,
    /// Arguments passed to the command.
    pub args: Vec<String>,
    /// Print the command's source instead of running it. SPEC §10.7.
    pub insert: bool,
    /// List commands instead of running one. SPEC §10.1.
    pub list: bool,
    /// Run the command and offer its output for selection. SPEC §10.7.
    pub select: bool,
}

/// The evaluation context for a project's config. SPEC §7.2, §7.3.
///
/// A free function rather than a method so callers can hold it while
/// borrowing the picker and runner mutably.
pub fn context<'a>(env: &'a Environment, config: &'a Config, cache: &'a Path) -> Context<'a> {
    Context {
        config,
        cache_dir: cache,
        shell: env.shell.as_deref(),
        direnv_dir: env.direnv_dir.as_deref(),
    }
}

/// Everything the subcommands run against. ENGINEERING §4.2.
pub struct App<P: Picker, R: ProcessRunner> {
    /// The effective configuration, before any local config is merged.
    pub config: Config,
    /// XDG directories.
    pub dirs: Dirs,
    /// The environment nixon was invoked in.
    pub env: Environment,
    /// How selections are made.
    pub picker: P,
    /// How commands are run.
    pub runner: R,
}

impl<P: Picker, R: ProcessRunner> App<P, R> {
    /// Builds an app.
    pub const fn new(config: Config, dirs: Dirs, env: Environment, picker: P, runner: R) -> Self {
        Self {
            config,
            dirs,
            env,
            picker,
            runner,
        }
    }

    /// The project containing the current directory, or it as a project.
    /// SPEC §9.5.
    pub fn current_project(&self) -> Project {
        find_in_project_or_default(&self.config.project_types, &self.env.cwd)
    }

    /// Every project under the configured source directories. SPEC §9.6.
    pub fn projects(&self) -> Vec<Project> {
        let home = self.dirs.home.clone();
        let expansion = discover::Expansion {
            home: &home,
            var: &|name| std::env::var(name).ok(),
        };
        discover::get_sorted_projects(
            &self.config.project_types,
            &self.config.project_dirs,
            &expansion,
        )
    }

    /// The config for a project: the global one with its local one merged on
    /// top. SPEC §3.1.
    pub fn config_for(&self, project: &Project) -> Result<Config> {
        Ok(load::find_local(&project.path())?.map_or_else(
            || self.config.clone(),
            |local| self.config.clone().merge(local),
        ))
    }

    /// Every command offered in a project, local config applied. SPEC §5.5.
    pub fn commands_for(&self, project: &Project) -> Result<Vec<Command>> {
        let config = self.config_for(project)?;
        Ok(find_project_commands(&config, project))
    }

    /// Finds a command by name among a project's commands. SPEC §5.5.
    pub fn find_named(commands: &[Command], name: &str) -> Result<Command> {
        commands
            .iter()
            .find(|command| command.name == name)
            .cloned()
            .ok_or_else(|| NixonError::UnknownCommand {
                name: name.to_owned(),
            })
    }
}
