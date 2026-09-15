function smith --description "Agent Smith (~/.claude)"
  env CLAUDE_CONFIG_DIR="$HOME/.claude" claude $argv
end
