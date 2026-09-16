function smith --description "Agent Smith (~/.smith)"
  env CLAUDE_CONFIG_DIR="$HOME/.smith" claude $argv
end
