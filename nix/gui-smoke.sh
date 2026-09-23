#!/usr/bin/env bash
# Drive the packaged GUI through both picker paths without running a command.
set -euo pipefail

binary=${1:?pass the wrapped nixon binary}
work=$(mktemp -d)
xvfb_pid=
gui_pid=

fail() {
  printf 'GUI smoke check: %s\n' "$*" >&2
  if [[ -f $work/ocr.txt ]]; then
    printf 'Last screen text:\n' >&2
    cat "$work/ocr.txt" >&2
  fi
  if [[ -f $work/gui.log ]]; then
    printf 'GUI output:\n' >&2
    cat "$work/gui.log" >&2
  fi
  if [[ -f $work/xvfb.log ]]; then
    printf 'Xvfb output:\n' >&2
    cat "$work/xvfb.log" >&2
  fi
  if [[ -n ${DISPLAY:-} ]]; then
    printf 'Display: %s\n' "$DISPLAY" >&2
  fi
  exit 1
}

cleanup() {
  if [[ -n $gui_pid ]]; then
    kill -- "-$gui_pid" 2>/dev/null || true
    wait "$gui_pid" 2>/dev/null || true
  fi
  if [[ -n $xvfb_pid ]]; then
    kill "$xvfb_pid" 2>/dev/null || true
    wait "$xvfb_pid" 2>/dev/null || true
  fi
  rm -rf "$work"
}
trap cleanup EXIT
trap 'exit 130' INT
trap 'exit 143' TERM

export HOME="$work/home"
export XDG_CONFIG_HOME="$work/config"
export XDG_CACHE_HOME="$work/cache"
export XDG_DATA_HOME="$work/data"
export XDG_STATE_HOME="$work/state"
export XDG_RUNTIME_DIR="$work/runtime"
export LIBGL_ALWAYS_SOFTWARE=1
export GUI_SMOKE_CURRENT_MARKER="$work/current/never-run-current"
export GUI_SMOKE_PROJECT_MARKER="$work/projects/MapleProject/never-run-project"
unset WAYLAND_DISPLAY WAYLAND_SOCKET XDG_SESSION_TYPE
mkdir -p "$HOME" "$XDG_CONFIG_HOME" "$XDG_CACHE_HOME" "$XDG_DATA_HOME" \
  "$XDG_STATE_HOME" "$XDG_RUNTIME_DIR" "$work/current/.git" \
  "$work/projects/MapleProject/.git"
chmod 700 "$XDG_RUNTIME_DIR"

cat > "$XDG_CONFIG_HOME/nixon.md" <<EOF
\`\`\`yaml config
project_dirs: ["$work/projects"]
project_types:
  - name: fixture
    desc: Fixture project
    test: [".git"]
\`\`\`
EOF
cat > "$work/current/nixon.md" <<'EOF'
### `Cedar`

```bash
touch "$GUI_SMOKE_CURRENT_MARKER"
```

### `Oak`

```bash
touch "$GUI_SMOKE_CURRENT_MARKER"
```
EOF
cat > "$work/projects/MapleProject/nixon.md" <<'EOF'
### `Birch`

```bash
touch "$GUI_SMOKE_PROJECT_MARKER"
```

### `Spruce`

```bash
touch "$GUI_SMOKE_PROJECT_MARKER"
```
EOF

cd "$work/current"
"$binary" run --list > "$work/commands.txt" 2> "$work/list-errors.txt" \
  || fail "could not list fixture commands: $(cat "$work/list-errors.txt")"
grep -Fxq Cedar "$work/commands.txt" || fail 'current fixture command was not found'
grep -Fxq Oak "$work/commands.txt" || fail 'second current fixture command was not found'
"$binary" project --list > "$work/projects.txt" 2> "$work/list-errors.txt" \
  || fail "could not list fixture projects: $(cat "$work/list-errors.txt")"
grep -Fq MapleProject "$work/projects.txt" || fail 'fixture project was not found'
cd "$work/projects/MapleProject"
"$binary" run --list > "$work/project-commands.txt" 2> "$work/list-errors.txt" \
  || fail "could not list project fixture commands: $(cat "$work/list-errors.txt")"
grep -Fxq Birch "$work/project-commands.txt" || fail 'project fixture command was not found'
grep -Fxq Spruce "$work/project-commands.txt" || fail 'second project fixture command was not found'
cd "$work/current"

# Avoid :0, which can be a host Xwayland display even when Xvfb's lock file
# is absent in a sandbox. Retry a high display number if another check has it.
for _ in {1..20}; do
  display_number=$((100 + RANDOM % 20000))
  export DISPLAY=":$display_number"
  Xvfb "$DISPLAY" -screen 0 1024x768x24 -nolisten tcp -ac \
    > "$work/xvfb.log" 2>&1 &
  xvfb_pid=$!
  for _ in {1..50}; do
    kill -0 "$xvfb_pid" 2>/dev/null || break
    if xdotool getdisplaygeometry > /dev/null 2>&1; then
      break 2
    fi
    sleep 0.1
  done
  kill "$xvfb_pid" 2>/dev/null || true
  wait "$xvfb_pid" 2>/dev/null || true
  xvfb_pid=
done
[[ -n $xvfb_pid ]] || fail 'could not start Xvfb on a free display'

setsid "$binary" --mode gui > "$work/gui.log" 2>&1 &
gui_pid=$!
window=
for _ in {1..300}; do
  window=$(xdotool search --onlyvisible --name '^Nixon$' 2>/dev/null | head -n 1 || true)
  [[ -n $window ]] && break
  kill -0 "$gui_pid" 2>/dev/null || fail "nixon exited before opening its window"
  sleep 0.1
done
[[ -n $window ]] || fail "timed out waiting for the Nixon window"
xdotool windowfocus --sync "$window" || fail "could not focus the Nixon window"

screen_has() {
  rm -f "$work/screen.png"
  scrot -u "$work/screen.png" || fail "could not capture the GUI window"
  tesseract "$work/screen.png" stdout --psm 11 > "$work/ocr.txt" 2> "$work/ocr.log" \
    || fail "could not read the GUI window: $(cat "$work/ocr.log")"
  grep -Fqi -- "$1" "$work/ocr.txt"
}

expect_screen() {
  local stage=$1 expected=$2
  for _ in {1..40}; do
    screen_has "$expected" && return
    kill -0 "$gui_pid" 2>/dev/null || fail "nixon exited during $stage"
    sleep 0.25
  done
  fail "timed out waiting for $stage (expected '$expected')"
}

expect_screen 'root menu' 'Commands'
xdotool key --clearmodifiers c
expect_screen 'current command picker' 'Select command [current]'
xdotool key --clearmodifiers Escape
expect_screen 'menu after canceling Commands' 'Commands'
xdotool key --clearmodifiers p
expect_screen 'project picker' 'Select project'
xdotool key --clearmodifiers Return
expect_screen 'selected project command picker' 'Select command [MapleProject]'
xdotool key --clearmodifiers Escape
expect_screen 'menu after canceling project command' 'Commands'
xdotool key --clearmodifiers Escape

for _ in {1..60}; do
  kill -0 "$gui_pid" 2>/dev/null || break
  sleep 0.1
done
kill -0 "$gui_pid" 2>/dev/null && fail "nixon did not close after the final Escape"
if ! wait "$gui_pid"; then
  fail "nixon exited with an error after the final Escape"
fi
if kill -0 -- "-$gui_pid" 2>/dev/null; then
  fail "nixon left a process in its process group"
fi
gui_pid=
[[ ! -e $GUI_SMOKE_CURRENT_MARKER ]] || fail 'the current command was launched'
[[ ! -e $GUI_SMOKE_PROJECT_MARKER ]] || fail 'the project command was launched'
printf 'GUI opened, navigated both pickers, and closed cleanly.\n'
