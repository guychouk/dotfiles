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
model=$(echo "$input" | jq -r '.model.id // empty')

# reasoning effort level (only present when the current model supports it)
effort=$(echo "$input" | jq -r '.effort.level // empty')
effort_part=""
effort_col=""
case "$effort" in
    low)    effort_part="●○○○○"; effort_col=$'\033[38;5;108m' ;;
    medium) effort_part="●●○○○"; effort_col=$'\033[38;5;220m' ;;
    high)   effort_part="●●●○○"; effort_col=$'\033[38;5;214m' ;;
    xhigh)  effort_part="●●●●○"; effort_col=$'\033[38;5;208m' ;;
    max)    effort_part="●●●●●"; effort_col=$'\033[38;5;203m' ;;
esac

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

# persona indicator (diane vs smith), from CLAUDE_CONFIG_DIR
persona_part=""
case "$CLAUDE_CONFIG_DIR" in
    */.diane) persona_part="💬" ;;
    */.smith) persona_part="😎" ;;
esac

# agent / remote-control indicator
agent_name=$(echo "$input" | jq -r '.agent.name // empty')
name_part=""
if [ -n "$agent_name" ]; then
    name_part="🕹️ $agent_name"
fi

# 5-hour session limit usage and reset time
five_pct=$(echo "$input" | jq -r '.rate_limits.five_hour.used_percentage // empty')
session_part=""
if [ -n "$five_pct" ]; then
    session_part=$(printf '⏳ %s%.0f%%%s' "$(pct_color "$five_pct")" "$five_pct" "$RESET")
fi

# 7-day (weekly) rate limit usage and reset time
week_pct=$(echo "$input" | jq -r '.rate_limits.seven_day.used_percentage // empty')
week_part=""
if [ -n "$week_pct" ]; then
    week_part=$(printf '🔄 %s%.0f%%%s' "$(pct_color "$week_pct")" "$week_pct" "$RESET")
fi

# context token usage
used_pct=$(echo "$input" | jq -r '.context_window.used_percentage // empty')
token_part=""
if [ -n "$used_pct" ]; then
    token_part=$(printf '🧠 %s%.0f%%%s' "$(pct_color "$used_pct")" "$used_pct" "$RESET")
fi

line=""
[ -n "$persona_part" ] && add_part "$persona_part"
[ -n "$token_part" ] && add_part "$token_part"
[ -n "$session_part" ] && add_part "$session_part"
[ -n "$week_part" ] && add_part "$week_part"
[ -n "$effort_part" ] && add_part "${effort_col}${effort_part}${RESET}"
[ -n "$model" ] && add_part "${COL_MODEL}${model}${RESET}"
[ -n "$dir_part" ] && add_part "${COL_DIR}${dir_part}${RESET}"
[ -n "$name_part" ] && add_part "${COL_NAME}${name_part}${RESET}"

printf '%s' "$line"
