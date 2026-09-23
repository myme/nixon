//! Typed configuration for the graphical launcher's menu.

use std::collections::HashSet;

use serde::{Deserialize, Deserializer};

/// Launcher fields supplied by one config source.
#[derive(Clone, Debug, Default, Eq, PartialEq)]
pub struct LauncherConfig {
    /// Terminal executable and its fixed arguments.
    pub terminal: Option<Vec<String>>,
    /// URL template used for browser searches.
    pub search_url: Option<String>,
    /// Root menu; a supplied tree replaces the inherited tree.
    pub items: Option<Vec<MenuItem>>,
}

impl LauncherConfig {
    /// The menu and search URL available without a config file.
    pub fn defaults() -> Self {
        Self {
            search_url: Some("https://www.google.com/search?q={query}".to_owned()),
            items: Some(vec![
                MenuItem::action(
                    MenuKey::Character('C'),
                    "Commands",
                    LauncherAction::Commands,
                ),
                MenuItem::action(
                    MenuKey::Character('P'),
                    "Projects",
                    LauncherAction::Projects,
                ),
                MenuItem::action(MenuKey::Character('H'), "History", LauncherAction::History),
                MenuItem::submenu(
                    MenuKey::Character('W'),
                    "Browser",
                    vec![MenuItem::action(
                        MenuKey::Character('O'),
                        "Open URL or search",
                        LauncherAction::BrowserInput,
                    )],
                ),
                MenuItem::submenu(
                    MenuKey::Character('S'),
                    "Spotify",
                    vec![
                        MenuItem::action(
                            MenuKey::Space,
                            "Play/Pause",
                            LauncherAction::Mpris {
                                operation: MprisOperation::PlayPause,
                                player: "spotify".to_owned(),
                            },
                        ),
                        MenuItem::action(
                            MenuKey::Character('P'),
                            "Previous",
                            LauncherAction::Mpris {
                                operation: MprisOperation::Previous,
                                player: "spotify".to_owned(),
                            },
                        ),
                        MenuItem::action(
                            MenuKey::Character('N'),
                            "Next",
                            LauncherAction::Mpris {
                                operation: MprisOperation::Next,
                                player: "spotify".to_owned(),
                            },
                        ),
                    ],
                ),
            ]),
            ..Self::default()
        }
    }

    /// Merges supplied launcher fields over inherited fields.
    #[must_use]
    pub fn merge(self, rhs: Self) -> Self {
        Self {
            terminal: rhs.terminal.or(self.terminal),
            search_url: rhs.search_url.or(self.search_url),
            items: rhs.items.or(self.items),
        }
    }
}

/// A single ASCII letter or digit, or the Space mnemonic.
#[derive(Clone, Debug, Eq, PartialEq)]
pub enum MenuKey {
    /// The configured letter or digit, preserving case for display.
    Character(char),
    /// The space bar.
    Space,
}

impl MenuKey {
    /// A canonical form for sibling uniqueness checks.
    #[must_use]
    pub const fn normalized(&self) -> char {
        match self {
            Self::Character(c) => c.to_ascii_uppercase(),
            Self::Space => ' ',
        }
    }
}

/// A menu branch or an action leaf.
#[derive(Clone, Debug, Eq, PartialEq)]
pub enum MenuItem {
    /// A menu with children.
    Submenu {
        /// Shortcut at the parent level.
        key: MenuKey,
        /// Visible name.
        label: String,
        /// Optional supporting text.
        description: Option<String>,
        /// Child entries.
        items: Vec<Self>,
    },
    /// An executable action.
    Action {
        /// Shortcut at the parent level.
        key: MenuKey,
        /// Visible name.
        label: String,
        /// Optional supporting text.
        description: Option<String>,
        /// The selected action.
        action: LauncherAction,
    },
}

impl MenuItem {
    fn action(key: MenuKey, label: &str, action: LauncherAction) -> Self {
        Self::Action {
            key,
            label: label.to_owned(),
            description: None,
            action,
        }
    }

    fn submenu(key: MenuKey, label: &str, items: Vec<Self>) -> Self {
        Self::Submenu {
            key,
            label: label.to_owned(),
            description: None,
            items,
        }
    }

    /// The shortcut at this menu level.
    #[must_use]
    pub const fn key(&self) -> &MenuKey {
        match self {
            Self::Submenu { key, .. } | Self::Action { key, .. } => key,
        }
    }
}

/// A launcher action, without execution policy.
#[derive(Clone, Debug, Eq, PartialEq)]
pub enum LauncherAction {
    /// Pick a command in the current project.
    Commands,
    /// Pick a project and then a command.
    Projects,
    /// Pick a history entry.
    History,
    /// Prompt for a URL or search text.
    BrowserInput,
    /// Control a media player.
    Mpris {
        /// Player operation.
        operation: MprisOperation,
        /// MPRIS player name.
        player: String,
    },
    /// Run a named Nixon command.
    Command {
        /// Command name, resolved when selected.
        name: String,
        /// Optional project path, resolved when selected.
        project: Option<String>,
    },
}

/// Media operations supported by the launcher.
#[derive(Clone, Copy, Debug, Eq, PartialEq, Deserialize)]
pub enum MprisOperation {
    /// Toggle playback.
    PlayPause,
    /// Skip to the previous track.
    Previous,
    /// Skip to the next track.
    Next,
}

/// Launcher fields from JSON or YAML, before semantic validation.
#[derive(Debug, Deserialize, Eq, PartialEq)]
#[serde(deny_unknown_fields)]
pub struct LauncherConfigSpec {
    terminal: Option<Vec<String>>,
    search_url: Option<String>,
    items: Option<Vec<MenuItemSpec>>,
}

#[derive(Debug, Deserialize, Eq, PartialEq)]
#[serde(deny_unknown_fields)]
struct MenuItemSpec {
    #[serde(deserialize_with = "deserialize_key")]
    key: String,
    label: String,
    description: Option<String>,
    action: Option<ActionSpec>,
    items: Option<Vec<Self>>,
}

fn deserialize_key<'de, D: Deserializer<'de>>(deserializer: D) -> Result<String, D::Error> {
    #[derive(Deserialize)]
    #[serde(untagged)]
    enum KeyValue {
        Text(String),
        Number(u8),
    }

    match KeyValue::deserialize(deserializer)? {
        KeyValue::Text(key) => Ok(key),
        KeyValue::Number(digit) => Ok(digit.to_string()),
    }
}

#[derive(Debug, Deserialize, Eq, PartialEq)]
#[serde(untagged)]
enum ActionSpec {
    Simple(SimpleAction),
    Mpris(MprisActionSpec),
    Command(CommandActionSpec),
}

#[derive(Debug, Deserialize, Eq, PartialEq)]
#[serde(rename_all = "snake_case")]
enum SimpleAction {
    Commands,
    Projects,
    History,
    BrowserInput,
}

#[derive(Debug, Deserialize, Eq, PartialEq)]
#[serde(deny_unknown_fields)]
struct MprisActionSpec {
    mpris: MprisOperation,
    player: String,
}

#[derive(Debug, Deserialize, Eq, PartialEq)]
#[serde(deny_unknown_fields)]
struct CommandActionSpec {
    command: String,
    project: Option<String>,
}

impl TryFrom<LauncherConfigSpec> for LauncherConfig {
    type Error = String;

    fn try_from(spec: LauncherConfigSpec) -> Result<Self, Self::Error> {
        if let Some(terminal) = &spec.terminal
            && (terminal.is_empty() || terminal.iter().any(|part| part.trim().is_empty()))
        {
            return Err("launcher.terminal: expected a nonempty argv".to_owned());
        }
        if spec
            .search_url
            .as_ref()
            .is_some_and(|url| url.trim().is_empty())
        {
            return Err("launcher.search_url: expected a nonempty URL".to_owned());
        }
        let items = spec
            .items
            .map(|items| parse_items(items, "launcher.items"))
            .transpose()?;
        Ok(Self {
            terminal: spec.terminal,
            search_url: spec.search_url,
            items,
        })
    }
}

fn parse_items(items: Vec<MenuItemSpec>, path: &str) -> Result<Vec<MenuItem>, String> {
    if items.is_empty() {
        return Err(format!("{path}: menu must have at least one item"));
    }
    let mut seen = HashSet::new();
    items
        .into_iter()
        .enumerate()
        .map(|(index, item)| {
            let item_path = format!("{path}[{index}] ({})", item.label);
            let key =
                parse_key(&item.key).map_err(|reason| format!("{item_path}.key: {reason}"))?;
            if !seen.insert(key.normalized()) {
                return Err(format!("{path}: duplicate sibling key '{}'", item.key));
            }
            if item.label.trim().is_empty() {
                return Err(format!("{item_path}.label: expected a nonempty label"));
            }
            match (item.action, item.items) {
                (Some(action), None) => Ok(MenuItem::Action {
                    key,
                    label: item.label,
                    description: item.description,
                    action: parse_action(action, &item_path)?,
                }),
                (None, Some(children)) => Ok(MenuItem::Submenu {
                    key,
                    label: item.label,
                    description: item.description,
                    items: parse_items(children, &item_path)?,
                }),
                _ => Err(format!(
                    "{item_path}: expected exactly one of action or items"
                )),
            }
        })
        .collect()
}

fn parse_action(action: ActionSpec, path: &str) -> Result<LauncherAction, String> {
    match action {
        ActionSpec::Simple(SimpleAction::Commands) => Ok(LauncherAction::Commands),
        ActionSpec::Simple(SimpleAction::Projects) => Ok(LauncherAction::Projects),
        ActionSpec::Simple(SimpleAction::History) => Ok(LauncherAction::History),
        ActionSpec::Simple(SimpleAction::BrowserInput) => Ok(LauncherAction::BrowserInput),
        ActionSpec::Mpris(spec) => {
            if spec.player.trim().is_empty() {
                return Err(format!("{path}.action.player: expected a nonempty player"));
            }
            Ok(LauncherAction::Mpris {
                operation: spec.mpris,
                player: spec.player,
            })
        }
        ActionSpec::Command(spec) => {
            if spec.command.trim().is_empty() {
                return Err(format!(
                    "{path}.action.command: expected a nonempty command"
                ));
            }
            if spec
                .project
                .as_ref()
                .is_some_and(|project| project.trim().is_empty())
            {
                return Err(format!("{path}.action.project: expected a nonempty path"));
            }
            Ok(LauncherAction::Command {
                name: spec.command,
                project: spec.project,
            })
        }
    }
}

fn parse_key(key: &str) -> Result<MenuKey, &'static str> {
    if key == "Space" {
        return Ok(MenuKey::Space);
    }
    let mut chars = key.chars();
    match (chars.next(), chars.next()) {
        (Some(c), None) if c.is_ascii_alphanumeric() => Ok(MenuKey::Character(c)),
        _ => Err("expected one ASCII letter or digit, or Space"),
    }
}

#[cfg(test)]
mod tests {
    use super::{LauncherAction, LauncherConfig, MenuItem, MenuKey, MprisOperation};
    use crate::config::{Config, parse_block};

    fn json(source: &str) -> Config {
        parse_block("json", source).unwrap()
    }

    fn yaml(source: &str) -> Config {
        parse_block("yaml", source).unwrap()
    }

    #[test]
    fn defaults_include_the_core_and_poc_actions() {
        let config = Config::defaults();
        let items = config.launcher.items.unwrap();
        assert_eq!(
            items
                .iter()
                .map(|item| item.key().normalized())
                .collect::<Vec<_>>(),
            vec!['C', 'P', 'H', 'W', 'S']
        );
        assert_eq!(
            config.launcher.search_url.as_deref(),
            Some("https://www.google.com/search?q={query}")
        );
        assert!(matches!(
            &items[4],
            MenuItem::Submenu { items, .. }
                if matches!(&items[0], MenuItem::Action {
                    key: MenuKey::Space,
                    action: LauncherAction::Mpris { operation: MprisOperation::PlayPause, player },
                    ..
                } if player == "spotify")
        ));
    }

    #[test]
    fn yaml_parses_submenus_actions_and_optional_descriptions() {
        let config = yaml(
            "launcher:\n  terminal: [kitty, -e]\n  items:\n    - key: w\n      label: Web\n      description: Browse\n      items:\n        - key: Space\n          label: Search\n          action: browser_input\n    - key: 7\n      label: Edit\n      action: { command: edit, project: ../notes }\n",
        );
        assert_eq!(
            config.launcher.terminal,
            Some(vec!["kitty".to_owned(), "-e".to_owned()])
        );
        let items = config.launcher.items.unwrap();
        assert!(
            matches!(&items[0], MenuItem::Submenu { key: MenuKey::Character('w'), description: Some(description), items, .. }
            if description == "Browse" && matches!(&items[0], MenuItem::Action { key: MenuKey::Space, action: LauncherAction::BrowserInput, .. }))
        );
        assert!(
            matches!(&items[1], MenuItem::Action { key: MenuKey::Character('7'), description: None, action: LauncherAction::Command { name, project: Some(project) }, .. }
            if name == "edit" && project == "../notes")
        );
    }

    #[test]
    fn json_parses_mpris_and_command_without_project() {
        let config = json(
            r#"{"launcher":{"items":[{"key":"P","label":"Pause","action":{"mpris":"PlayPause","player":"spotify"}},{"key":"E","label":"Edit","action":{"command":"edit"}}]}}"#,
        );
        let items = config.launcher.items.unwrap();
        assert!(
            matches!(&items[0], MenuItem::Action { action: LauncherAction::Mpris { operation: MprisOperation::PlayPause, player }, .. } if player == "spotify")
        );
        assert!(
            matches!(&items[1], MenuItem::Action { action: LauncherAction::Command { name, project: None }, .. } if name == "edit")
        );
    }

    #[test]
    fn launcher_merge_inherits_fields_and_replaces_items() {
        let global = Config::defaults().merge(json(r#"{"launcher":{"terminal":["kitty","-e"],"search_url":"https://example.test/?q={query}","items":[{"key":"A","label":"A","action":"commands"}]}}"#));
        let local = json(
            r#"{"launcher":{"search_url":"https://local.test/?q={query}","items":[{"key":"B","label":"B","action":"history"}]}}"#,
        );
        let merged = global.merge(local);
        assert_eq!(
            merged.launcher.terminal,
            Some(vec!["kitty".to_owned(), "-e".to_owned()])
        );
        assert_eq!(
            merged.launcher.search_url.as_deref(),
            Some("https://local.test/?q={query}")
        );
        assert_eq!(
            merged.launcher.items.unwrap()[0].key(),
            &MenuKey::Character('B')
        );
    }

    #[test]
    fn omitted_local_items_keep_the_inherited_menu() {
        let global = Config::defaults();
        let local = json(r#"{"launcher":{"terminal":["foot","-e"]}}"#);
        assert_eq!(
            global.clone().merge(local).launcher.items,
            global.launcher.items
        );
    }

    #[test]
    fn invalid_menu_shapes_report_the_menu_path() {
        for source in [
            r#"{"launcher":{"items":[]}}"#,
            r#"{"launcher":{"items":[{"key":"A","label":"A","items":[]}]}}"#,
            r#"{"launcher":{"items":[{"key":"A","label":"A"}]}}"#,
            r#"{"launcher":{"items":[{"key":"A","label":"A","action":"history","items":[{"key":"B","label":"B","action":"commands"}]}]}}"#,
            r#"{"launcher":{"items":[{"key":"A","label":"A","action":"history"},{"key":"a","label":"Again","action":"commands"}]}}"#,
            r#"{"launcher":{"items":[{"key":"Space","label":"A","action":"history"},{"key":"Space","label":"Again","action":"commands"}]}}"#,
            r#"{"launcher":{"items":[{"key":"Å","label":"A","action":"history"}]}}"#,
            r#"{"launcher":{"items":[{"key":"","label":"A","action":"history"}]}}"#,
            r#"{"launcher":{"items":[{"key":"AB","label":"A","action":"history"}]}}"#,
            r#"{"launcher":{"items":[{"key":"A","label":"  ","action":"history"}]}}"#,
        ] {
            let error = parse_block("json", source).unwrap_err().to_string();
            assert!(error.contains("launcher.items"), "{error}");
        }
    }

    #[test]
    fn keys_can_be_reused_at_different_menu_levels() {
        let config = json(
            r#"{"launcher":{"items":[{"key":"A","label":"First","items":[{"key":"a","label":"Nested","action":"history"}]},{"key":"B","label":"Second","items":[{"key":"a","label":"Nested","action":"commands"}]}]}}"#,
        );
        assert_eq!(config.launcher.items.unwrap().len(), 2);
    }

    #[test]
    fn unknown_launcher_keys_actions_and_arguments_fail() {
        for source in [
            r#"{"launcher":{"serch_url":"typo"}}"#,
            r#"{"launcher":{"items":[{"key":"A","label":"A","actoin":"history"}]}}"#,
            r#"{"launcher":{"items":[{"key":"A","label":"A","action":"unknown"}]}}"#,
            r#"{"launcher":{"items":[{"key":"A","label":"A","action":{"mpris":"Stop","player":"spotify"}}]}}"#,
            r#"{"launcher":{"items":[{"key":"A","label":"A","action":{"command":"edit","typo":1}}]}}"#,
            r#"{"launcher":{"items":[{"key":"A","label":"A","action":{"command":" "}}]}}"#,
            r#"{"launcher":{"items":[{"key":"A","label":"A","action":{"mpris":"Next","player":" "}}]}}"#,
        ] {
            assert!(parse_block("json", source).is_err(), "{source}");
        }
    }

    #[test]
    fn invalid_terminal_and_blank_project_fail() {
        for source in [
            r#"{"launcher":{"terminal":[]}}"#,
            r#"{"launcher":{"terminal":["kitty", " "]}}"#,
            r#"{"launcher":{"items":[{"key":"E","label":"Edit","action":{"command":"edit","project":" "}}]}}"#,
        ] {
            assert!(parse_block("json", source).is_err(), "{source}");
        }
    }

    #[test]
    fn unknown_top_level_keys_stay_tolerated() {
        assert_eq!(
            json(r#"{"launcher":{},"typo":true}"#).launcher,
            LauncherConfig::default()
        );
    }
}
