#!/bin/bash

input=$(cat)

RESET=$'\033[0m'
COL_MODEL=$'\033[38;5;110m'
COL_NAME=$'\033[38;5;140m'
COL_DIR=$'\033[38;5;67m'
COL_DIM=$'\033[38;5;8m'

# green under 50%, yellow under 80%, red at/above 80%
pct_color() {
    local p_int
    p_int=$(printf '%.0f' "${1:-0}")
    if [ "$p_int" -ge 80 ]; then
        printf '%s' $'\033[38;5;203m'
    elif [ "$p_int" -ge 50 ]; then
        printf '%s' $'\033[38;5;179m'
    else
        printf '%s' $'\033[38;5;108m'
    fi
}

format_tokens() {
    local num=$1
    if [ "$num" -ge 1000 ]; then
        printf '%dk' $((num / 1000))
    else
        printf '%d' "$num"
    fi
}

add_part() {
    if [ -n "$1" ]; then
        if [ -n "$line" ]; then
            line="${line} ${COL_DIM}·${RESET} $1"
        else
            line="$1"
        fi
    fi
}

# current model
model=$(echo "$input" | jq -r '.model.display_name // empty')

# reasoning effort level (only present when the current model supports it)
effort=$(echo "$input" | jq -r '.effort.level // empty')
effort_part=""
[ -n "$effort" ] && effort_part="🎚️ $effort"

# current directory (and project, if it differs, e.g. inside a worktree)
current_dir=$(echo "$input" | jq -r '.workspace.current_dir // empty')
project_dir=$(echo "$input" | jq -r '.workspace.project_dir // empty')
dir_part=""
if [ -n "$current_dir" ]; then
    dir_name=$(basename "$current_dir")
    if [ -n "$project_dir" ] && [ "$project_dir" != "$current_dir" ]; then
        dir_part="📁 ${dir_name} (${COL_DIM}$(basename "$project_dir")${RESET}${COL_DIR})"
    else
        dir_part="📁 ${dir_name}"
    fi
fi

# session name (/rename) and agent / remote-control indicator
session_name=$(echo "$input" | jq -r '.session_name // empty')
agent_name=$(echo "$input" | jq -r '.agent.name // empty')
name_part=""
[ -n "$session_name" ] && name_part="🏷️ $session_name"
if [ -n "$agent_name" ]; then
    [ -n "$name_part" ] && name_part="$name_part  "
    name_part="${name_part}🕹️ $agent_name"
fi

# 5-hour session limit usage and reset time
five_pct=$(echo "$input" | jq -r '.rate_limits.five_hour.used_percentage // empty')
five_reset=$(echo "$input" | jq -r '.rate_limits.five_hour.resets_at // empty')
session_part=""
if [ -n "$five_pct" ]; then
    reset_str=""
    [ -n "$five_reset" ] && reset_str=" ($(date -r "${five_reset%.*}" '+%H:%M'))"
    session_part=$(printf '⏳ %s%.0f%%%s%s' "$(pct_color "$five_pct")" "$five_pct" "$RESET" "$reset_str")
fi

# 7-day (weekly) rate limit usage and reset time
week_pct=$(echo "$input" | jq -r '.rate_limits.seven_day.used_percentage // empty')
week_reset=$(echo "$input" | jq -r '.rate_limits.seven_day.resets_at // empty')
week_part=""
if [ -n "$week_pct" ]; then
    reset_str=""
    [ -n "$week_reset" ] && reset_str=" ($(date -r "${week_reset%.*}" '+%a %H:%M'))"
    week_part=$(printf '📅 %s%.0f%%%s%s' "$(pct_color "$week_pct")" "$week_pct" "$RESET" "$reset_str")
fi

# context token usage and limit
window_size=$(echo "$input" | jq -r '.context_window.context_window_size // empty')
used_pct=$(echo "$input" | jq -r '.context_window.used_percentage // empty')
token_part=""
if [ -n "$window_size" ] && [ -n "$used_pct" ]; then
    window_fmt=$(format_tokens "$window_size")
    token_part=$(printf '🧠 %s%.0f%%%s/%s' "$(pct_color "$used_pct")" "$used_pct" "$RESET" "$window_fmt")
fi

line=""
[ -n "$model" ] && add_part "${COL_MODEL}${model}${RESET}"
[ -n "$effort_part" ] && add_part "${COL_DIM}${effort_part}${RESET}"
[ -n "$dir_part" ] && add_part "${COL_DIR}${dir_part}${RESET}"
[ -n "$name_part" ] && add_part "${COL_NAME}${name_part}${RESET}"
[ -n "$session_part" ] && add_part "$session_part"
[ -n "$week_part" ] && add_part "$week_part"
[ -n "$token_part" ] && add_part "$token_part"

printf '%s' "$line"
