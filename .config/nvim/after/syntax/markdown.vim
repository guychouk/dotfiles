" This extends the markdown syntax file with support for YAML metadata

syn region markdownYaml start="---" end="---"
hi def link markdownYaml Comment
