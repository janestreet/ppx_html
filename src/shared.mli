open! Core
open Ppxlib

type modul := longident loc option

val node_fn : loc:location -> html_syntax_module:modul -> label -> expression
val attr_fn : loc:location -> html_syntax_module:modul -> label -> expression
val attr_fn_with_create : loc:location -> html_syntax_module:modul -> label -> expression
val attr_t_type : loc:location -> core_type
val node_t_type : loc:location -> core_type
