if exists("b:current_syntax")
    finish
endif

syntax keyword IPGKeyword EOI where repeat until start end for to do
highlight link IPGKeyword Keyword

syntax match IPGDelimiter "?\[\|:\|?\|(\|)\|\[\|\]\|,\|;\|_\|{\|}"
highlight link IPGDelimiter Delimiter

syntax match IPGOperator "\v\*\*"
syntax match IPGOperator "\v\<\<"
syntax match IPGOperator "\v\>\>"
syntax match IPGOperator "\v\<\="
syntax match IPGOperator "\v\>\="
syntax match IPGOperator "\v\<"
syntax match IPGOperator "\v\>"
syntax match IPGOperator "\v-\>"
syntax match IPGOperator "\v\=\="
syntax match IPGOperator "\v!\="
syntax match IPGOperator "\v!"
syntax match IPGOperator "\v\="
syntax match IPGOperator "\v\&\&"
syntax match IPGOperator "\v\|\|"
syntax match IPGOperator "\v\&"
syntax match IPGOperator "\v\|"
syntax match IPGOperator "\v\^"
syntax match IPGOperator "\v\%"
syntax match IPGOperator "\v\~"
syntax match IPGOperator "\v\+"
syntax match IPGOperator "\v-"
syntax match IPGOperator "\v\*"
syntax match IPGOperator "\v/"
highlight link IPGOperator Operator

" TODO: Use the regex from the lexer
syntax region IPGString start=/\v"/ skip=/\v\\./ end=/\v"/
highlight link IPGString String

syntax keyword IPGTodo TODO FIXME XXX TBD contained
highlight link IPGTodo Todo

syntax match IPGComment "//.*$" contains=IPGTodo
highlight link IPGComment Comment

let b:current_syntax = "ipg"
