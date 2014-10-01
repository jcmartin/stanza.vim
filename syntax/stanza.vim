" Vim syntax file
" Language:    Stanza (http://lbstanza.org/)
" Maintainers: James Martin, Patrick Li
" Last Change: 2014 September 30
"   2014 September 23 : Initial Stanza Syntax File
"   2014 September 29 : Modified for literals using backtick (`).
"   2014 September 30 : Begrudgingly add support for other characters in
"                       keywords such as * and +. When Stanza supports
"                       unicode, they will need to be added as well.
"                       Stanza literals are broken again....

if version < 600
    syntax clear
elseif exists("b:current_syntax")
    finish
endif

" Stanza lets users use more than just alphanumerics and _ in identifiers
set iskeyword=@,48-57,_,-,+,*,/,=,!,?,@,$,>,<,%,#,~,.

syn keyword stanzaConditional if else switch match case when
syn keyword stanzaRepeat      for while
syn keyword stanzaOperator    and in to is not or new through by let
syn keyword stanzaScopeDecl   public
syn keyword stanzaEmptyArg    _
syn keyword stanzaInfixOperators == != + - / * $ = >= <= > < % << >>
syn match   stanzaInfixOperators "\(\<[A-Z][a-zA-Z0-9_']*\.\)\=[-!#$%&\*\+/<=>\?@\\^|~.][-!#$%&\*\+/<=>\?@\\^|~:.]*"
syn match   stanzaInfixOperators "\(\<[A-Z][a-zA-Z0-9_']*\.\)\=:[-!#$%&\*\+./<=>\?@\\^|~:]*"
syn keyword stanzaException   throw try catch

syn keyword stanzaInclude     import include

syn match   stanzaComment    ";.*$" contains=stanzaTodo,@Spell
syn keyword stanzaTodo        FIXME NOTE NOTES TODO XXX contained

syn keyword stanzaInclude      include      nextgroup=stanzaIncludeFile skipwhite

syn region stanzaIncludeFile start="<" end=">" contained contains=stanzaString skipwhite

" Stanza Literals
syn match  stanzaLiteral "`[^ (),\[\]]\+"
syn region stanzaLiteral matchgroup=stanzaLiteral start="`(" end=")" contains=stanzaLiteralInsideParen,stanzaLiteralInside
syn region stanzaLiteral matchgroup=stanzaLiteral start="`\[" end="\]" contains=stanzaLiteralInsideParen,stanzaLiteralInside
"syn region stanzaLiteralInsideParen matchgroup=stanzaLiteralInsideParen start="(" end=")" contains=stanzaLiteralInsideParen,stanzaLiteralInside contained
syn match  stanzaLiteralInside ".\{-}" contained

syn region stanzaChar matchgroup=stanzaChar start="\'" end="\'" contains=stanzaStringEscape

" Identifiers
syn match stanzaIdentifier "[^ ,"'`;&|<>\\:()\[\]\{\}]\+" nextgroup=stanzaColonType,stanzaAs,stanzaSquareBrackets skipwhite
syn match stanzaColonType ":"                contained   nextgroup=stanzaTupleType,stanzaType skipwhite
"syn region stanzaSquareBrackets start="\[" end="\]" contained skipwhite

" Casting
syn keyword stanzaAs as contained nextgroup=stanzaType skipwhite

" Setter Function
syn keyword stanzaSetter setter nextgroup=stanzaEqualArrow skipwhite
syn match  stanzaEqualArrow "=>" contained nextgroup=stanzaSetterFunctionName skipwhite
syn match stanzaSetterFunctionName "[^ \]\\$\!=:;([\"]\+" contained

" Types
syn match  stanzaType    "[^ ()-<>|=:;\[\]]\+\|()" contained nextgroup=stanzaTypeAnd,stanzaArrow,stanzaOr,stanzaTypeParameter,stanzaSubType skipwhite
syn region stanzaTypeParameter matchgroup=stanzaTypeParameter start="<" end=">"   contained contains=stanzaType,stanzaTupleType nextgroup=stanzaTypeAnd,stanzaArrow,stanzaOr,stanzaTypeParameter,stanzaSubType skipwhite
syn region stanzaTupleType matchgroup=stanzaTupleType start="\[" end="\]"     contained contains=stanzaType,stanzaTupleType nextgroup=stanzaTypeAnd,stanzaArrow,stanzaOr,stanzaTypeParameter,stanzaSubType skipwhite
syn match  stanzaSubType "<:"                      contained                     nextgroup=stanzaType,stanzaTupleType skipwhite
syn match  stanzaOr      "|"                       contained                     nextgroup=stanzaType,stanzaTupleType skipwhite
syn match  stanzaArrow   "->"                      contained                     nextgroup=stanzaType,stanzaTupleType skipwhite
syn match  stanzaTypeAnd "&"                       contained                     nextgroup=stanzaType,stanzaTupleType skipwhite skipnl

" Label
syn keyword stanzaLabel        label        nextgroup=stanzaDefNameParameterTypes


" definitions
syn keyword stanzaFn           fn           nextgroup=stanzaArgumentList skipwhite
syn keyword stanzaDef          defn defn*   nextgroup=stanzaDefName skipwhite
syn keyword stanzaDefPackage   defpackage   nextgroup=stanzaDefPackageName skipwhite
syn keyword stanzaVal          val          nextgroup=stanzaValName skipwhite
syn keyword stanzaVar          var          nextgroup=stanzaVarName skipwhite
syn keyword stanzaDefClass     defclass     nextgroup=stanzaType,stanzaTupleType skipwhite
syn keyword stanzaDefInterface definterface nextgroup=stanzaType,stanzaTupleType skipwhite
syn keyword stanzaDefMulti     defmulti     nextgroup=stanzaDefName skipwhite
syn keyword stanzaDefInterface defmethod    nextgroup=stanzaDefName skipwhite
syn keyword stanzaDefStruct    defstruct    nextgroup=stanzaTupleType,stanzaType skipwhite

syn match stanzaValName        "[^ \\$\!=:;([\"]\+" contained nextgroup=stanzaColonType skipwhite
syn match stanzaVarName        "[^ \\$\!=:;([\"]\+" contained nextgroup=stanzaColonType skipwhite
syn match stanzaDefPackageName "[^ \\$\!=:;([\"]\+" contained

" Function Name etc
syn match  stanzaDefName   "[^ <>:()]\+"                     contained nextgroup=stanzaDefNameParameterTypes,stanzaArgumentList skipwhite
syn region stanzaDefNameParameterTypes matchgroup=stanzaDefNameParameterTypes start="<" end=">" contained contains=stanzaTupleType,stanzaType nextgroup=stanzaArgumentList skipwhite
syn region stanzaArgumentList matchgroup=stanzaArgumentList start="(" end=")"            contained contains=stanzaArgument nextgroup=stanzaArrow skipwhite
syn match  stanzaArgument      "[^ ,()&:]\+"               contained nextgroup=stanzaArgumentColon skipwhite
syn match  stanzaArgumentComma ","                         contained nextgroup=stanzaArgument skipwhite
syn match  stanzaArgumentColon ":"                         contained nextgroup=stanzaArgumentOutsideType,stanzaArgumentOutsideTupleType,stanzaArgumentOutsideTypeParen skipwhite



" Everything below is to handle the fact that argument commas are different
" from type commas
syn match  stanzaArgumentOutsideType "[^ -$!<>|,=:;()\[ ]\+\|()" contained nextgroup=stanzaArgumentOutsideOr,stanzaArgumentOutsideTypeParameter,stanzaArgumentOutsideArrow,stanzaArgumentComma skipwhite
syn match  stanzaArgumentOutsideOr   "|"                         contained nextgroup=stanzaArgumentOutsideType,stanzaArgumentOutsideTupleType,stanzaArgumentOutsideTypeParen skipwhite
syn match  stanzaArgumentOutsideAnd  "&"                         contained nextgroup=stanzaArgumentOutsideType,stanzaArgumentOutsideTupleType,stanzaArgumentOutsideTypeParen skipwhite
syn match  stanzaArgumentOutsideArrow "->"                       contained nextgroup=stanzaArgumentOutsideType,stanzaArgumentOutsideTupleType,stanzaArgumentOutsideTypeParen skipwhite
syn region stanzaArgumentOutsideTypeParameter start="<" end=">"  contained contains=stanzaArgumentType,stanzaArgumentTupleType,stanzaArgumentTypeParen nextgroup=stanzaArgumentOutsideOr,stanzaArgumentOutsideAnd,stanzaArgumentOutsideTypeParameter,stanzaArgumentOutsideArrow,stanzaArgumentComma skipwhite
syn region stanzaArgumentTupleType start="\[" end="\]"           contained contains=stanzaArgumentType,stanzaArgumentTupleType,stanzaArgumentTypeParen 
            \ nextgroup=stanzaArgumentOutsideTypeOr,stanzaArgumentOutsideAnd,stanzaArgumentOutsideTypeParameter,stanzaArgumentOutsideArrow,stanzaArgumentComma skipwhite
syn region stanzaArgumentOutsideTypeParen matchgroup=stanzaArgumentOutsideTypeParen start="(" end=")"      contained contains=stanzaArgumentOutsideType,stanzaArgumentTupleOutsideType,stanzaArgumentOutsideTypeParen 
            \ nextgroup=stanzaArgumentOutsideTypeOr,stanzaArgumentOutsideAnd,stanzaArgumentOutsideTypeParameter,stanzaArgumentOutsideArrow,stanzaArgumentComma skipwhite


syn match  stanzaArgumentArrow "->"                        contained nextgroup=stanzaArgumentType,stanzaArgumentTupleType,stanzaArgumentTypeParen skipwhite
syn match  stanzaArgumentTypeOr "|"                        contained nextgroup=stanzaArgumentType,stanzaArgumentTupleType,stanzaArgumentTypeParen skipwhite
syn region stanzaArgumentTypeParameter start="<" end=">"   contained contains= stanzaArgumentType,stanzaArgumentTupleType,stanzaArgumentTypeParen skipwhite

syn region stanzaArgumentType matchgroup=stanzaArgumentType start="(" end=")"            contained contains=stanzaArgumentType,stanzaArgumentTupleType,stanzaArgumentTypeParen 
            \ nextgroup=stanzaArgumentTypeOr,stanzaArgumentTypeParameter,stanzaArgumentArrow,stanzaArgumentComma,stanzaArgumentTypeAnd skipwhite
syn region stanzaArgumentTupleType matchgroup=stanzaArgumentTupleType start="\[" end="\]"     contained contains=stanzaArgumentType,stanzaArgumentTupleType,stanzaArgumentTypeParen 
            \ nextgroup=stanzaArgumentTypeOr,stanzaArgumentTypeParameter,stanzaArgumentArrow,stanzaArgumentComma,stanzaArgumentTypeAnd skipwhite
syn match  stanzaArgumentType  "[^ -$!<>|,=:;()\[ ]\+\|()" contained nextgroup=stanzaArgumentTypeOr,stanzaArgumentTypeParameter,stanzaArgumentArrow,stanzaArgumentTypeComma,stanzaArgumentTypeAnd skipwhite
syn match  stanzaArgumentTypeComma ","                     contained nextgroup=stanzaArgumentType,stanzaArgumentTupleType,stanzaArgumentTypeParen skipwhite
syn match  stanzaArgumentTypeAnd   "&"                     contained nextgroup=stanzaArgumentType,stanzaArgumentTupleType,stanzaArgumentTypeParen skipwhite


if !exists("stanza_no_builtin_highlight")
  " built-in constants
  syn keyword stanzaBuiltin	false true
  " built-in functions
  syn keyword stanzaBuiltin	print print-all println println-all
  syn keyword stanzaBuiltin length
  syn keyword stanzaBuiltin list head headn tail tailn
  syn keyword stanzaBuiltin do map
  syn keyword stanzaBuiltin equal? less? greater? less-eq? greater-eq?
  syn keyword stanzaBuiltin substring
  syn keyword stanzaBuiltin get set
  syn keyword stanzaBuiltin exit error identical? complement commandline-arguments procedure-path procedure-dir call-system
  syn keyword stanzaBuiltin file-exists? read-file write-file 
  syn keyword stanzaBuiltin rand max min shift-left shift-right bit-or bit-xor bit-and bit-inv bit-set? neg abs
  syn keyword stanzaBuiltin plus minus times divide modulo ceil-log2
  syn keyword stanzaBuiltin to-string to-int to-char to-float to-array to-stream
  syn keyword stanzaBuiltin lo hi sin cos tan asin acos atan atan2 sqrt pow log log10 exp ceil floor round
  syn keyword stanzaBuiltin more? calc-next close next peek
  syn keyword stanzaBuiltin $do-prim
  syn keyword stanzaBuiltin current-coroutine parent winder set-winder windouts windins call-windins
  syn keyword stanzaBuiltin send break open?
endif

" string literals with escapes
syn region stanzaString start="\"[^"]" skip="\\\"" end="\"" contains=scalaStringEscape
syn match stanzaStringEscape "\\u[0-9a-fA-F]\{4}" contained
syn match stanzaStringEscape "\\[nrfvb\\\"\']" contained

" number literals
if !exists("stanza_no_number_highlight")
  syn match stanzaNumber "\<\d\+\>"
  syn match stanzaNumber "\<\d\+\.\d+\>"
endif

" Sync at the beginning of class, function, or method definition.
syn sync match stanzaSync grouphere NONE "^\s*\%(defn\|defmulti\|defmethod\|defclass\)\s\+\h\w*\s*("

if version >= 508 || !exists("did_stanza_syn_inits")
  if version <= 508
    let did_stanza_syn_inits = 1
    command -nargs=+ HiLink hi link <args>
  else
    command -nargs=+ HiLink hi def link <args>
  endif


  " The default highlight links.  Can be overridden later.
  HiLink stanzaStatement    Statement
  HiLink stanzaConditional  Conditional
  HiLink stanzaRepeat       Repeat
  HiLink stanzaOperator     Operator
  HiLink stanzaException    Exception
  HiLink stanzaInclude      Include
  HiLink stanzaDecorator    Define
  HiLink stanzaFunction     Function
  HiLink stanzaStorageClass StorageClass
  HiLink stanzaScopeDecl    Keyword
  HiLink stanzaTodo         Todo
  HiLink stanzaString       String
  HiLink stanzaRawString    String
  HiLink stanzaEscape       Special
  HiLink stanzaChar         Character
  HiLink stanzaStringEscape Character

  " Keywords
  HiLink stanzaEmptyArg     Keyword

  " Labels
  HiLink stanzaLabel        Keyword

  " Related to definitions
  HiLink stanzaFn           Keyword
  HiLink stanzaDef          Keyword
  HiLink stanzaDefName      Function
  HiLink stanzaDefNameParameterTypes Keyword
  HiLink stanzaVal          Keyword
"  HiLink stanzaValName      Identifier
  HiLink stanzaVar          Keyword
"  HiLink stanzaVarName      Identifier
  HiLink stanzaDefPackage   Keyword
  HiLink stanzaDefClass     Keyword
  HiLink stanzaDefMulti     Keyword
  HiLink stanzaDefMethod    Keyword
  HiLink stanzaDefInterface Keyword
  HiLink stanzaDefStruct    Keyword

  " Cast
  HiLink stanzaAs           Keyword

  " Setter
  HiLink stanzaSetter       Keyword
  HiLink stanzaEqualArrow   Keyword
  HiLink stanzaSetterFunctionName Function

  " Type Keywords
  HiLink stanzaType          Type
  HiLink stanzaTupleType     Keyword
  HiLink stanzaArrow         Keyword
  HiLink stanzaOr            Keyword
  HiLink stanzaColonType     Keyword
  HiLink stanzaSubType       Keyword
  HiLink stanzaTypeParameter Keyword
  HiLink stanzaTypeAnd       Keyword

  HiLink stanzaInfixOperators   Operator

"  HiLink stanzaIdentifier   Identifier

  " Stanza Arguments
"  HiLink stanzaArgument     Identifier
  HiLink stanzaArgumentType         Type
  HiLink stanzaArgumentColon        Keyword
  HiLink stanzaArgumentTypeParameter Keyword
  HiLink stanzaArgumentTypeOr       Keyword
  HiLink stanzaArgumentTypeAnd      Keyword
  HiLink stanzaArgumentArrow        Keyword
  HiLink stanzaArgumentTupleType    Keyword

  HiLink stanzaArgumentOutsideType          Type
  HiLink stanzaArgumentOutsideOr            Keyword
  HiLink stanzaArgumentOutsideAnd           Keyword
  HiLink stanzaArgumentOutsideArrow         Keyword
  HiLink stanzaArgumentOutsideTupleType     Keyword
  HiLink stanzaArgumentOutsideTypeParameter Keyword

  HiLink stanzaComment      Comment
  HiLink stanzaIncludeFile  Keyword

  HiLink stanzaLiteral                  Constant
  HiLink stanzaLiteralInside            Constant
  HiLink stanzaLiteralInsideParen       Constant

  if !exists("stanza_no_number_highlight")
    HiLink stanzaNumber     Number
  endif
  if !exists("stanza_no_builtin_highlight")
    HiLink stanzaBuiltin    Function
  endif
  if !exists("stanza_no_exception_highlight")
    HiLink stanzaExceptions Structure
  endif
  if exists("stanza_space_error_highlight")
    HiLink stanzaSpaceError Error
  endif
  if !exists("stanza_no_doctest_highlight")
    HiLink stanzaDoctest    Special
    HiLink stanzaDoctestValue    Define
  endif

  delcommand HiLink
endif

let b:current_syntax = "stanza"
