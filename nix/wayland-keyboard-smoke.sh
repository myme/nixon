#!/usr/bin/env bash
# Drive the packaged Wayland GUI with keys forwarded through nested Weston.
set -euo pipefail

binary=${1:?pass the wrapped nixon binary}
weston=${2:?pass the Weston binary}
work=$(mktemp -d)
xvfb_pid=
weston_pid=
gui_pid=

cleanup() {
  for pid in "$gui_pid" "$weston_pid" "$xvfb_pid"; do
    if [[ -n $pid ]]; then
      kill -TERM "$pid" 2>/dev/null || true
      wait "$pid" 2>/dev/null || true
    fi
  done
  rm -rf "$work"
}
trap cleanup EXIT
trap 'exit 143' TERM

fail() {
  printf 'Wayland keyboard smoke check: %s\n' "$*" >&2
  if [[ -f $work/ocr.txt ]]; then
    printf 'Last screen text:\n' >&2
    cat "$work/ocr.txt" >&2
  fi
  for log in "$work/gui.stderr" "$work/weston.log" "$work/xvfb.log"; do
    if [[ -f $log ]]; then
      printf '%s (last 30 lines):\n' "$log" >&2
      tail -30 "$log" >&2
    fi
  done
  exit 1
}

export HOME="$work/home"
export XDG_CONFIG_HOME="$work/config"
export XDG_CACHE_HOME="$work/cache"
export XDG_DATA_HOME="$work/data"
export XDG_STATE_HOME="$work/state"
export XDG_RUNTIME_DIR="$work/runtime"
export WAYLAND_DISPLAY=nixon-wayland
export LIBGL_ALWAYS_SOFTWARE=1
export WAYLAND_SMOKE_MARKER="$work/never-run"
unset XDG_SESSION_TYPE WAYLAND_SOCKET
mkdir -p "$HOME" "$XDG_CONFIG_HOME" "$XDG_CACHE_HOME" "$XDG_DATA_HOME" \
  "$XDG_STATE_HOME" "$XDG_RUNTIME_DIR" "$work/current/.git"
chmod 700 "$XDG_RUNTIME_DIR"

cat > "$work/current/nixon.md" <<'EOF'
### `Cedar`

```bash
touch "$WAYLAND_SMOKE_MARKER"
```

### `Oak`

```bash
touch "$WAYLAND_SMOKE_MARKER"
```
EOF
cd "$work/current"
"$binary" run --list > "$work/commands.txt" 2> "$work/list.stderr" \
  || fail "could not list fixture commands: $(cat "$work/list.stderr")"
rg -Fxq Cedar "$work/commands.txt" || fail 'Cedar fixture command was not found'
rg -Fxq Oak "$work/commands.txt" || fail 'Oak fixture command was not found'

# Retry high Xvfb display numbers to avoid a host Xwayland display.
for _ in {1..10}; do
  export DISPLAY=":$((1000 + RANDOM % 20000))"
  Xvfb "$DISPLAY" -screen 0 1280x900x24 -nolisten tcp -ac \
    > "$work/xvfb.log" 2>&1 &
  xvfb_pid=$!
  for _ in {1..30}; do
    xdotool getdisplaygeometry > /dev/null 2>&1 && break 2
    kill -0 "$xvfb_pid" 2>/dev/null || break
    sleep 0.1
  done
  kill -TERM "$xvfb_pid" 2>/dev/null || true
  wait "$xvfb_pid" 2>/dev/null || true
  xvfb_pid=
done
[[ -n $xvfb_pid ]] || fail 'Xvfb did not start on a free display'

"$weston" --backend=x11 --renderer=pixman --socket="$WAYLAND_DISPLAY" \
  --no-config --idle-time=0 --width=1024 --height=768 \
  --log="$work/weston.log" > "$work/weston.stdout" 2> "$work/weston.stderr" &
weston_pid=$!
for _ in {1..50}; do
  [[ -S $XDG_RUNTIME_DIR/$WAYLAND_DISPLAY ]] && break
  kill -0 "$weston_pid" 2>/dev/null || break
  sleep 0.2
done
[[ -S $XDG_RUNTIME_DIR/$WAYLAND_DISPLAY ]] || fail 'Weston did not create its Wayland socket'

# Weston's X11 output window is reported before its title is searchable.
window=
for _ in {1..50}; do
  window=$(rg -o 'window id [0-9]+' "$work/weston.log" | tail -n 1 | cut -d' ' -f3 || true)
  [[ -n $window ]] && xdotool getwindowgeometry "$window" > /dev/null 2>&1 && break
  sleep 0.2
done
if [[ -z $window ]] || ! xdotool getwindowgeometry "$window" > /dev/null 2>&1; then
  fail 'Weston did not create its X11 output window'
fi
xdotool windowfocus --sync "$window" || fail 'could not focus Weston output'

env -u DISPLAY -u XDG_SESSION_TYPE WAYLAND_DEBUG=1 "$binary" --mode gui \
  > "$work/gui.stdout" 2> "$work/gui.stderr" &
gui_pid=$!
for _ in {1..50}; do
  rg -q 'xdg_surface[^[:space:]]*\.get_toplevel' "$work/gui.stderr" && break
  kill -0 "$gui_pid" 2>/dev/null || fail 'nixon exited before opening a Wayland window'
  sleep 0.2
done
rg -q 'xdg_surface[^[:space:]]*\.get_toplevel' "$work/gui.stderr" \
  || fail 'nixon did not request a Wayland toplevel'

screen_has() {
  local expected=$1
  rm -f "$work/screen.png" "$work/screen-ocr.png" "$work/screen-inverted.png"
  scrot -u "$work/screen.png" || fail 'could not capture Weston output'
  ffmpeg -v error -y -i "$work/screen.png" \
    -vf 'scale=iw*3:ih*3:flags=lanczos,eq=contrast=2' -frames:v 1 \
    "$work/screen-ocr.png" || fail 'could not prepare screenshot for OCR'
  tesseract "$work/screen-ocr.png" stdout --psm 11 \
    > "$work/ocr.txt" 2> "$work/ocr.log" || fail 'could not OCR Weston output'
  if rg -Fqi "$expected" "$work/ocr.txt"; then
    return 0
  fi
  ffmpeg -v error -y -i "$work/screen-ocr.png" -vf negate -frames:v 1 \
    "$work/screen-inverted.png" || fail 'could not invert screenshot for OCR'
  tesseract "$work/screen-inverted.png" stdout --psm 11 \
    > "$work/ocr-inverted.txt" 2> "$work/ocr.log" || fail 'could not OCR inverted screenshot'
  if rg -Fqi "$expected" "$work/ocr-inverted.txt"; then
    cp "$work/ocr-inverted.txt" "$work/ocr.txt"
    return 0
  fi
  return 1
}

expect_screen() {
  local stage=$1 expected=$2
  for _ in {1..20}; do
    screen_has "$expected" && return
    kill -0 "$gui_pid" 2>/dev/null || fail "nixon exited during $stage"
    sleep 0.25
  done
  fail "timed out waiting for $stage (expected '$expected')"
}

expect_screen 'root menu' 'Commands'
xdotool key --clearmodifiers c
expect_screen 'command picker' 'Select command'
rg -q 'wl_keyboard[^[:space:]]*\.key\(' "$work/gui.stderr" \
  || fail 'nixon received no Wayland keyboard event'
xdotool key --clearmodifiers Escape
expect_screen 'root after cancel' 'Commands'
xdotool key --clearmodifiers Escape

for _ in {1..50}; do
  kill -0 "$gui_pid" 2>/dev/null || break
  sleep 0.1
done
kill -0 "$gui_pid" 2>/dev/null && fail 'nixon did not close after final Escape'
wait "$gui_pid" || fail 'nixon exited with an error after final Escape'
gui_pid=
[[ ! -s $work/gui.stdout ]] || fail 'nixon wrote to stdout'
[[ ! -e $WAYLAND_SMOKE_MARKER ]] || fail 'fixture command was executed'
printf 'Wayland GUI opened a command picker, canceled, and closed cleanly.\n'
