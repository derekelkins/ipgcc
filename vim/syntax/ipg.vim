if exists("b:current_syntax")
    finish
endif

syntax keyword IPGKeyword EOI where repeat until START END for to do starting on true false
highlight link IPGKeyword Keyword

syntax match IPGDelimiter "?\[\|:\|?\|(\|)\|\[\|\]\|,\|;\|{\|}"
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

syntax match IPGMalformedString /\v"([^"\\]|\\.)*"/
highlight link IPGMalformedString Error

syntax match IPGString /\v"([^"\\]|\\[0abfnrtv\\"']|\\x[0-9a-fA-F][0-9a-fA-F])*"/ contains=IPGEscape
highlight link IPGString String

syntax match IPGEscape /\v\\[0abfnrtv\\\"']/ contained
syntax match IPGEscape /\v\\x[0-9a-fA-F][0-9a-fA-F]/ contained
highlight link IPGEscape Special

syntax match IPGDeclare /\v\%end/
syntax match IPGDeclare /\v^\%declare/
syntax match IPGDeclare /\v^\%instrument/
syntax match IPGDeclare /\v^\%preamble_end/
syntax match IPGDeclare /\v^\%postamble_begin/
highlight link IPGDeclare PreProc

syntax keyword IPGTodo TODO FIXME XXX TBD contained
highlight link IPGTodo Todo

syntax match IPGComment "//.*$" contains=IPGTodo
highlight link IPGComment Comment

syntax region IPGMultilineComment start="\v/\*" end="\v\*/" contains=IPGMultilineComment,IPGTodo
highlight link IPGMultilineComment Comment

let b:current_syntax = "ipg"
