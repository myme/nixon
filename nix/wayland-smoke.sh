#!/usr/bin/env bash
# Verify the packaged GUI creates and renders a Wayland window under Weston.
set -euo pipefail

binary=${1:?pass the wrapped nixon binary}
weston=${2:?pass the Weston binary}
work=$(mktemp -d)
weston_pid=
gui_pid=

cleanup() {
  if [[ -n $gui_pid ]]; then
    kill -TERM "$gui_pid" 2>/dev/null || true
    wait "$gui_pid" 2>/dev/null || true
  fi
  if [[ -n $weston_pid ]]; then
    kill -TERM "$weston_pid" 2>/dev/null || true
    wait "$weston_pid" 2>/dev/null || true
  fi
  rm -rf "$work"
}
trap cleanup EXIT
trap 'exit 143' TERM

fail() {
  printf 'Wayland GUI smoke check: %s\n' "$*" >&2
  for log in "$work/gui.stderr" "$work/weston.log" "$work/weston.stderr"; do
    if [[ -f $log ]]; then
      printf '%s (last 50 lines):\n' "$log" >&2
      tail -50 "$log" >&2
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
unset DISPLAY XDG_SESSION_TYPE WAYLAND_SOCKET
mkdir -p "$HOME" "$XDG_CONFIG_HOME" "$XDG_CACHE_HOME" "$XDG_DATA_HOME" \
  "$XDG_STATE_HOME" "$XDG_RUNTIME_DIR" "$work/current"
chmod 700 "$XDG_RUNTIME_DIR"
cd "$work/current"

"$weston" --backend=headless --renderer=pixman --socket="$WAYLAND_DISPLAY" \
  --no-config --idle-time=0 --width=800 --height=600 \
  --log="$work/weston.log" >"$work/weston.stdout" 2>"$work/weston.stderr" &
weston_pid=$!
for _ in {1..50}; do
  [[ -S $XDG_RUNTIME_DIR/$WAYLAND_DISPLAY ]] && break
  kill -0 "$weston_pid" 2>/dev/null || break
  sleep 0.2
done
[[ -S $XDG_RUNTIME_DIR/$WAYLAND_DISPLAY ]] || fail 'Weston did not create its socket'

WAYLAND_DEBUG=1 "$binary" --mode gui >"$work/gui.stdout" 2>"$work/gui.stderr" &
gui_pid=$!
for _ in {1..100}; do
  if rg -q 'xdg_surface[^[:space:]]*\.get_toplevel' "$work/gui.stderr" &&
    rg -q 'xdg_toplevel[^[:space:]]*\.configure\(' "$work/gui.stderr" &&
    rg -q 'wl_surface[^[:space:]]*\.attach\(wl_buffer' "$work/gui.stderr" &&
    rg -q 'wl_surface[^[:space:]]*\.commit\(' "$work/gui.stderr"; then
    break
  fi
  kill -0 "$gui_pid" 2>/dev/null || fail 'nixon exited before creating a window'
  sleep 0.2
done

rg -q 'xdg_surface[^[:space:]]*\.get_toplevel' "$work/gui.stderr" || fail 'no xdg_toplevel requested'
rg -q 'xdg_toplevel[^[:space:]]*\.configure\(' "$work/gui.stderr" || fail 'no toplevel configure event'
rg -q 'wl_surface[^[:space:]]*\.attach\(wl_buffer' "$work/gui.stderr" || fail 'no rendered buffer attached'
rg -q 'wl_surface[^[:space:]]*\.commit\(' "$work/gui.stderr" || fail 'no surface commit'
sleep 1
kill -0 "$gui_pid" 2>/dev/null || fail 'nixon exited after creating its window'
[[ ! -s $work/gui.stdout ]] || fail 'nixon wrote to stdout'

printf 'Wayland GUI opened and committed a rendered surface.\n'
