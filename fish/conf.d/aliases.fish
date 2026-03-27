# macos aliases
alias blender '/Applications/Blender.app/Contents/MacOS/Blender'
alias godot   '/Applications/Godot.app/Contents/MacOS/Godot'
alias brave   '/Applications/Brave Browser.app/Contents/MacOS/Brave Browser'

# listing files with ll
if command -q eza
  alias ll='eza -la --time-style=long-iso --group-directories-first --color=always'
else if command -q gls
  alias ll='gls -lah --group-directories-first --color=always'
else
  alias ll='ls -lah --color=always'
end
