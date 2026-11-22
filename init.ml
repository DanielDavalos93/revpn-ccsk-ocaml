(* Open utop with [utop -init init.ml] *)

#mod_use "revpn-ccsk/util.ml"
open Util
#mod_use "revpn-ccsk/ccsk.ml"
#mod_use "revpn-ccsk/lts.ml"
#mod_use "revpn-ccsk/net.ml"
open Net
#mod_use "revpn-ccsk/token.ml"
open Token
#mod_use "revpn-ccsk/enriching.ml"
open Enriching
#mod_use "test/token_test.ml"
(* open Token_test *)
#mod_use "test/reversing_nets.ml"
open Reversing_nets
