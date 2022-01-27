source <sfile>:h/typescript.vim

source <sfile>:h/jsx.vim

" refine the typescript line comment
syntax region typescriptLineComment start=+//+ end=/$/ contains=@Spell,typescriptCommentTodo,typescriptRef extend keepend

" add a typescriptBlock group for typescript
syntax region typescriptBlock
			\ matchgroup=typescriptBraces
			\ start="{"
			\ end="}"
			\ contained
			\ extend
			\ contains=@typescriptExpression,typescriptBlock
			\ fold
hi def link typescriptTypeBrackets typescriptOpSymbols

" Fix type casting ambiguity with JSX syntax
syntax match typescriptTypeBrackets +[<>]+ contained
syntax match typescriptTypeCast +<\([_$A-Za-z0-9]\+\)>\%(\s*\%([_$A-Za-z0-9]\+\s*;\?\|(\)\%(\_[^<]*</\1>\)\@!\)\@=+ contains=typescriptTypeBrackets,@typescriptType,typescriptType nextgroup=@typescriptExpression

syntax cluster typescriptExpression add=jsxRegion,typescriptParens

let b:current_syntax = 'typescriptreact'
