
module Basics = struct
  
  exception Error
  
  type token = 
    | XOR
    | VOID
    | UID of (string)
    | TYPE
    | TO
    | SUB
    | STRING of (string)
    | STORE
    | STAR
    | SLT
    | SLE
    | SHL
    | SGT
    | SGE
    | RPAREN
    | RET
    | RBRACKET
    | RBRACE
    | OR
    | NULL
    | NE
    | MUL
    | LSHR
    | LPAREN
    | LOAD
    | LBRACKET
    | LBRACE
    | LBL of (string)
    | LABEL
    | INT of (int)
    | ICMP
    | I8
    | I64
    | I32
    | I1
    | GLOBAL
    | GID of (string)
    | GEP
    | EQUALS
    | EQ
    | EOF
    | ENTRY
    | DEFINE
    | CROSS
    | COMMA
    | COLON
    | CALL
    | BR
    | BITCAST
    | ASHR
    | AND
    | ALLOCA
    | ADD
  
end

include Basics

let _eRR =
  Basics.Error

type _menhir_env = {
  _menhir_lexer: Lexing.lexbuf -> token;
  _menhir_lexbuf: Lexing.lexbuf;
  _menhir_token: token;
  mutable _menhir_error: bool
}

and _menhir_state = 
  | MenhirState172
  | MenhirState166
  | MenhirState157
  | MenhirState152
  | MenhirState151
  | MenhirState146
  | MenhirState144
  | MenhirState143
  | MenhirState142
  | MenhirState139
  | MenhirState138
  | MenhirState136
  | MenhirState135
  | MenhirState133
  | MenhirState131
  | MenhirState130
  | MenhirState128
  | MenhirState127
  | MenhirState124
  | MenhirState123
  | MenhirState121
  | MenhirState120
  | MenhirState118
  | MenhirState114
  | MenhirState113
  | MenhirState111
  | MenhirState109
  | MenhirState108
  | MenhirState105
  | MenhirState103
  | MenhirState99
  | MenhirState97
  | MenhirState94
  | MenhirState93
  | MenhirState92
  | MenhirState91
  | MenhirState89
  | MenhirState87
  | MenhirState86
  | MenhirState73
  | MenhirState72
  | MenhirState71
  | MenhirState70
  | MenhirState61
  | MenhirState57
  | MenhirState54
  | MenhirState53
  | MenhirState51
  | MenhirState50
  | MenhirState40
  | MenhirState39
  | MenhirState37
  | MenhirState36
  | MenhirState35
  | MenhirState32
  | MenhirState31
  | MenhirState28
  | MenhirState26
  | MenhirState22
  | MenhirState19
  | MenhirState17
  | MenhirState16
  | MenhirState11
  | MenhirState10
  | MenhirState5
   open Ll
   open Ll.Parsing


let rec _menhir_goto_ty_operand_list_rev : _menhir_env -> 'ttv_tail -> _menhir_state -> ((Ll.ty * Ll.operand) list) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    let _menhir_stack = Obj.magic _menhir_stack in
    assert (not _menhir_env._menhir_error);
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | COMMA ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_env = _menhir_discard _menhir_env in
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | I1 ->
            _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState113
        | I64 ->
            _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState113
        | I8 ->
            _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState113
        | LBRACE ->
            _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState113
        | LBRACKET ->
            _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState113
        | UID _v ->
            _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState113 _v
        | VOID ->
            _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState113
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState113)
    | RPAREN ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, (tos : ((Ll.ty * Ll.operand) list))) = _menhir_stack in
        let _v : ((Ll.ty * Ll.operand) list) =     ( List.rev tos ) in
        let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
        (match _menhir_s with
        | MenhirState111 ->
            let _menhir_stack = Obj.magic _menhir_stack in
            assert (not _menhir_env._menhir_error);
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | RPAREN ->
                let _menhir_stack = Obj.magic _menhir_stack in
                let _menhir_env = _menhir_discard _menhir_env in
                let _menhir_stack = Obj.magic _menhir_stack in
                let ((((_menhir_stack, _menhir_s, (u : (string))), _, (t : (Ll.ty))), _, (o : (Ll.operand))), _, (args : ((Ll.ty * Ll.operand) list))) = _menhir_stack in
                let _8 = () in
                let _6 = () in
                let _3 = () in
                let _2 = () in
                let _v : (Ll.uid * Ll.insn) =     ( (u, Call (t, o, args)) ) in
                _menhir_goto_insn _menhir_env _menhir_stack _menhir_s _v
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                let _menhir_stack = Obj.magic _menhir_stack in
                let (_menhir_stack, _menhir_s, _) = _menhir_stack in
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
        | MenhirState146 ->
            let _menhir_stack = Obj.magic _menhir_stack in
            assert (not _menhir_env._menhir_error);
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | RPAREN ->
                let _menhir_stack = Obj.magic _menhir_stack in
                let _menhir_env = _menhir_discard _menhir_env in
                let _menhir_stack = Obj.magic _menhir_stack in
                let ((((_menhir_stack, _menhir_s), _, (t : (Ll.ty))), _, (o : (Ll.operand))), _, (args : ((Ll.ty * Ll.operand) list))) = _menhir_stack in
                let _6 = () in
                let _4 = () in
                let _1 = () in
                let _v : (Ll.uid * Ll.insn) =     ( (gensym "call", Call (t, o, args)) ) in
                _menhir_goto_insn _menhir_env _menhir_stack _menhir_s _v
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                let _menhir_stack = Obj.magic _menhir_stack in
                let (_menhir_stack, _menhir_s, _) = _menhir_stack in
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
        | _ ->
            _menhir_fail ())
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s

and _menhir_reduce86 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _v : ((Ll.ty * Ll.operand) list) =     ( [] ) in
    _menhir_goto_ty_operand_list_rev _menhir_env _menhir_stack _menhir_s _v

and _menhir_goto_i_operand_list_rev : _menhir_env -> 'ttv_tail -> (Ll.operand list) -> 'ttv_return =
  fun _menhir_env _menhir_stack _v ->
    let _menhir_stack = (_menhir_stack, _v) in
    let _menhir_stack = Obj.magic _menhir_stack in
    assert (not _menhir_env._menhir_error);
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | COMMA ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_env = _menhir_discard _menhir_env in
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | I32 ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | GID _v ->
                _menhir_run77 _menhir_env (Obj.magic _menhir_stack) MenhirState105 _v
            | INT _v ->
                _menhir_run76 _menhir_env (Obj.magic _menhir_stack) MenhirState105 _v
            | NULL ->
                _menhir_run75 _menhir_env (Obj.magic _menhir_stack) MenhirState105
            | UID _v ->
                _menhir_run74 _menhir_env (Obj.magic _menhir_stack) MenhirState105 _v
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState105)
        | I64 ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | GID _v ->
                _menhir_run77 _menhir_env (Obj.magic _menhir_stack) MenhirState103 _v
            | INT _v ->
                _menhir_run76 _menhir_env (Obj.magic _menhir_stack) MenhirState103 _v
            | NULL ->
                _menhir_run75 _menhir_env (Obj.magic _menhir_stack) MenhirState103
            | UID _v ->
                _menhir_run74 _menhir_env (Obj.magic _menhir_stack) MenhirState103 _v
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState103)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            raise _eRR)
    | BR | CALL | RET | STORE | UID _ ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, (os : (Ll.operand list))) = _menhir_stack in
        let _v : (Ll.operand list) =     ( List.rev os ) in
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let (os : (Ll.operand list)) = _v in
        let (((((_menhir_stack, _menhir_s, (u : (string))), _, (_4 : (Ll.ty))), _), _, (t : (Ll.ty))), _, (o : (Ll.operand))) = _menhir_stack in
        let _8 = () in
        let _5 = () in
        let _3 = () in
        let _2 = () in
        let _v : (Ll.uid * Ll.insn) =     ( (u, Gep (t,o,os)) ) in
        _menhir_goto_insn _menhir_env _menhir_stack _menhir_s _v
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        let _menhir_stack = Obj.magic _menhir_stack in
        raise _eRR

and _menhir_goto_list_insn_ : _menhir_env -> 'ttv_tail -> _menhir_state -> ((Ll.uid * Ll.insn) list) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    match _menhir_s with
    | MenhirState172 | MenhirState61 | MenhirState142 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let (is : ((Ll.uid * Ll.insn) list)) = _v in
        let _v : ((Ll.uid * Ll.insn) list) =     ( is ) in
        let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | BR ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | I1 ->
                let _menhir_stack = Obj.magic _menhir_stack in
                let _menhir_env = _menhir_discard _menhir_env in
                let _tok = _menhir_env._menhir_token in
                (match _tok with
                | GID _v ->
                    _menhir_run77 _menhir_env (Obj.magic _menhir_stack) MenhirState157 _v
                | INT _v ->
                    _menhir_run76 _menhir_env (Obj.magic _menhir_stack) MenhirState157 _v
                | NULL ->
                    _menhir_run75 _menhir_env (Obj.magic _menhir_stack) MenhirState157
                | UID _v ->
                    _menhir_run74 _menhir_env (Obj.magic _menhir_stack) MenhirState157 _v
                | _ ->
                    assert (not _menhir_env._menhir_error);
                    _menhir_env._menhir_error <- true;
                    _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState157)
            | LABEL ->
                let _menhir_stack = Obj.magic _menhir_stack in
                let _menhir_env = _menhir_discard _menhir_env in
                let _tok = _menhir_env._menhir_token in
                (match _tok with
                | UID _v ->
                    let _menhir_stack = Obj.magic _menhir_stack in
                    let _menhir_env = _menhir_discard _menhir_env in
                    let _menhir_stack = Obj.magic _menhir_stack in
                    let (l : (string)) = _v in
                    let _2 = () in
                    let _1 = () in
                    let _v : (Ll.terminator) =     ( Br l ) in
                    _menhir_goto_terminator _menhir_env _menhir_stack _v
                | _ ->
                    assert (not _menhir_env._menhir_error);
                    _menhir_env._menhir_error <- true;
                    let _menhir_stack = Obj.magic _menhir_stack in
                    raise _eRR)
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                let _menhir_stack = Obj.magic _menhir_stack in
                raise _eRR)
        | RET ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | I1 ->
                _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState151
            | I64 ->
                _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState151
            | I8 ->
                _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState151
            | LBRACE ->
                _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState151
            | LBRACKET ->
                _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState151
            | UID _v ->
                _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState151 _v
            | VOID ->
                _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState151
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState151)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState166 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let (xs : ((Ll.uid * Ll.insn) list)) = _v in
        let (_menhir_stack, _menhir_s, (x : (Ll.uid * Ll.insn))) = _menhir_stack in
        let _v : ((Ll.uid * Ll.insn) list) =     ( x :: xs ) in
        _menhir_goto_list_insn_ _menhir_env _menhir_stack _menhir_s _v
    | _ ->
        _menhir_fail ()

and _menhir_goto_cnd : _menhir_env -> 'ttv_tail -> (Ll.cnd) -> 'ttv_return =
  fun _menhir_env _menhir_stack _v ->
    let _menhir_stack = (_menhir_stack, _v) in
    let _menhir_stack = Obj.magic _menhir_stack in
    assert (not _menhir_env._menhir_error);
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | I1 ->
        _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState86
    | I64 ->
        _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState86
    | I8 ->
        _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState86
    | LBRACE ->
        _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState86
    | LBRACKET ->
        _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState86
    | UID _v ->
        _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState86 _v
    | VOID ->
        _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState86
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState86

and _menhir_goto_bop : _menhir_env -> 'ttv_tail -> (Ll.bop) -> 'ttv_return =
  fun _menhir_env _menhir_stack _v ->
    let _menhir_stack = (_menhir_stack, _v) in
    let _menhir_stack = Obj.magic _menhir_stack in
    assert (not _menhir_env._menhir_error);
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | I1 ->
        _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState130
    | I64 ->
        _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState130
    | I8 ->
        _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState130
    | LBRACE ->
        _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState130
    | LBRACKET ->
        _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState130
    | UID _v ->
        _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState130 _v
    | VOID ->
        _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState130
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState130

and _menhir_goto_gdecl_list_rev : _menhir_env -> 'ttv_tail -> _menhir_state -> (Ll.gdecl list) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    let _menhir_stack = Obj.magic _menhir_stack in
    assert (not _menhir_env._menhir_error);
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | COMMA ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_env = _menhir_discard _menhir_env in
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | I1 ->
            _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState39
        | I64 ->
            _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState39
        | I8 ->
            _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState39
        | LBRACE ->
            _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState39
        | LBRACKET ->
            _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState39
        | UID _v ->
            _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState39 _v
        | VOID ->
            _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState39
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState39)
    | RBRACE | RBRACKET ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, (gs : (Ll.gdecl list))) = _menhir_stack in
        let _v : (Ll.gdecl list) =     ( List.rev gs ) in
        let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
        (match _menhir_s with
        | MenhirState37 ->
            let _menhir_stack = Obj.magic _menhir_stack in
            assert (not _menhir_env._menhir_error);
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | RBRACE ->
                let _menhir_stack = Obj.magic _menhir_stack in
                let _menhir_env = _menhir_discard _menhir_env in
                let _menhir_stack = Obj.magic _menhir_stack in
                let ((_menhir_stack, _menhir_s), _, (gs : (Ll.gdecl list))) = _menhir_stack in
                let _3 = () in
                let _1 = () in
                let _v : (Ll.ginit) =     ( GStruct gs ) in
                _menhir_goto_ginit _menhir_env _menhir_stack _menhir_s _v
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                let _menhir_stack = Obj.magic _menhir_stack in
                let (_menhir_stack, _menhir_s, _) = _menhir_stack in
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
        | MenhirState35 ->
            let _menhir_stack = Obj.magic _menhir_stack in
            assert (not _menhir_env._menhir_error);
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | RBRACKET ->
                let _menhir_stack = Obj.magic _menhir_stack in
                let _menhir_env = _menhir_discard _menhir_env in
                let _menhir_stack = Obj.magic _menhir_stack in
                let ((_menhir_stack, _menhir_s), _, (gs : (Ll.gdecl list))) = _menhir_stack in
                let _3 = () in
                let _1 = () in
                let _v : (Ll.ginit) =     ( GArray gs ) in
                _menhir_goto_ginit _menhir_env _menhir_stack _menhir_s _v
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                let _menhir_stack = Obj.magic _menhir_stack in
                let (_menhir_stack, _menhir_s, _) = _menhir_stack in
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
        | _ ->
            _menhir_fail ())
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s

and _menhir_goto_block_list_rev : _menhir_env -> 'ttv_tail -> ((Ll.lbl * Ll.block) list) -> 'ttv_return =
  fun _menhir_env _menhir_stack _v ->
    let _menhir_stack = (_menhir_stack, _v) in
    let _menhir_stack = Obj.magic _menhir_stack in
    assert (not _menhir_env._menhir_error);
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | LBL _v ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = (_menhir_stack, _v) in
        let _menhir_env = _menhir_discard _menhir_env in
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | COLON ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | CALL ->
                _menhir_run143 _menhir_env (Obj.magic _menhir_stack) MenhirState172
            | STORE ->
                _menhir_run135 _menhir_env (Obj.magic _menhir_stack) MenhirState172
            | UID _v ->
                _menhir_run62 _menhir_env (Obj.magic _menhir_stack) MenhirState172 _v
            | BR | RET ->
                _menhir_reduce60 _menhir_env (Obj.magic _menhir_stack) MenhirState172
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState172)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            raise _eRR)
    | RBRACE ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, (bs : ((Ll.lbl * Ll.block) list))) = _menhir_stack in
        let _v : ((Ll.lbl * Ll.block) list) =     ( List.rev bs ) in
        let _menhir_stack = (_menhir_stack, _v) in
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | RBRACE ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _menhir_stack = Obj.magic _menhir_stack in
            let (((((_menhir_stack, _, (t : (Ll.ty))), _, (l : (string))), _, (a : ((Ll.ty * Ll.uid) list))), _, (eb : (Ll.block))), (bs : ((Ll.lbl * Ll.block) list))) = _menhir_stack in
            let _10 = () in
            let _7 = () in
            let _6 = () in
            let _4 = () in
            let _1 = () in
            let _v : (Ll.gid * Ll.fdecl) =     ( (l, { fty = (List.map fst a, t)
          ; param = List.map snd a
          ; cfg = (eb, bs)
          }
    ) ) in
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_stack = Obj.magic _menhir_stack in
            let (f : (Ll.gid * Ll.fdecl)) = _v in
            let (_menhir_stack, (ds : (Ll.prog))) = _menhir_stack in
            let _v : (Ll.prog) =     ( { ds with fdecls = f :: ds.fdecls }  ) in
            _menhir_goto_decls_rev _menhir_env _menhir_stack _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s, _), _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        let _menhir_stack = Obj.magic _menhir_stack in
        raise _eRR

and _menhir_goto_entry_block : _menhir_env -> 'ttv_tail -> _menhir_state -> (Ll.block) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    let _menhir_stack = Obj.magic _menhir_stack in
    let _menhir_stack = Obj.magic _menhir_stack in
    let _v : ((Ll.lbl * Ll.block) list) =     ( [] ) in
    _menhir_goto_block_list_rev _menhir_env _menhir_stack _v

and _menhir_goto_operand : _menhir_env -> 'ttv_tail -> _menhir_state -> (Ll.operand) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    match _menhir_s with
    | MenhirState73 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let (((((_menhir_stack, _menhir_s, (u : (string))), _, (_4 : (Ll.ty))), _), _, (t : (Ll.ty))), _, (o : (Ll.operand))) = _menhir_stack in
        let _5 = () in
        let _3 = () in
        let _2 = () in
        let _v : (Ll.uid * Ll.insn) =     ( (u, Load (t,o)) ) in
        _menhir_goto_insn _menhir_env _menhir_stack _menhir_s _v
    | MenhirState87 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | COMMA ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | GID _v ->
                _menhir_run77 _menhir_env (Obj.magic _menhir_stack) MenhirState89 _v
            | INT _v ->
                _menhir_run76 _menhir_env (Obj.magic _menhir_stack) MenhirState89 _v
            | NULL ->
                _menhir_run75 _menhir_env (Obj.magic _menhir_stack) MenhirState89
            | UID _v ->
                _menhir_run74 _menhir_env (Obj.magic _menhir_stack) MenhirState89 _v
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState89)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState89 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let (((((_menhir_stack, _menhir_s, (u : (string))), (c : (Ll.cnd))), _, (t : (Ll.ty))), _, (o1 : (Ll.operand))), _, (o2 : (Ll.operand))) = _menhir_stack in
        let _7 = () in
        let _3 = () in
        let _2 = () in
        let _v : (Ll.uid * Ll.insn) =     ( (u, Icmp (c,t,o1,o2)) ) in
        _menhir_goto_insn _menhir_env _menhir_stack _menhir_s _v
    | MenhirState94 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | COMMA ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | I32 ->
                let _menhir_stack = Obj.magic _menhir_stack in
                let _menhir_env = _menhir_discard _menhir_env in
                let _tok = _menhir_env._menhir_token in
                (match _tok with
                | GID _v ->
                    _menhir_run77 _menhir_env (Obj.magic _menhir_stack) MenhirState99 _v
                | INT _v ->
                    _menhir_run76 _menhir_env (Obj.magic _menhir_stack) MenhirState99 _v
                | NULL ->
                    _menhir_run75 _menhir_env (Obj.magic _menhir_stack) MenhirState99
                | UID _v ->
                    _menhir_run74 _menhir_env (Obj.magic _menhir_stack) MenhirState99 _v
                | _ ->
                    assert (not _menhir_env._menhir_error);
                    _menhir_env._menhir_error <- true;
                    _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState99)
            | I64 ->
                let _menhir_stack = Obj.magic _menhir_stack in
                let _menhir_env = _menhir_discard _menhir_env in
                let _tok = _menhir_env._menhir_token in
                (match _tok with
                | GID _v ->
                    _menhir_run77 _menhir_env (Obj.magic _menhir_stack) MenhirState97 _v
                | INT _v ->
                    _menhir_run76 _menhir_env (Obj.magic _menhir_stack) MenhirState97 _v
                | NULL ->
                    _menhir_run75 _menhir_env (Obj.magic _menhir_stack) MenhirState97
                | UID _v ->
                    _menhir_run74 _menhir_env (Obj.magic _menhir_stack) MenhirState97 _v
                | _ ->
                    assert (not _menhir_env._menhir_error);
                    _menhir_env._menhir_error <- true;
                    _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState97)
            | BR | CALL | COMMA | RET | STORE | UID _ ->
                let _menhir_stack = Obj.magic _menhir_stack in
                let _v : (Ll.operand list) =     ( [] ) in
                _menhir_goto_i_operand_list_rev _menhir_env _menhir_stack _v
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                let _menhir_stack = Obj.magic _menhir_stack in
                let (_menhir_stack, _menhir_s, _) = _menhir_stack in
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState97 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _, (o : (Ll.operand))) = _menhir_stack in
        let _1 = () in
        let _v : (Ll.operand list) =       ( [o] ) in
        _menhir_goto_i_operand_list_rev _menhir_env _menhir_stack _v
    | MenhirState99 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _, (o : (Ll.operand))) = _menhir_stack in
        let _1 = () in
        let _v : (Ll.operand list) =     ( [o] ) in
        _menhir_goto_i_operand_list_rev _menhir_env _menhir_stack _v
    | MenhirState103 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let ((_menhir_stack, (os : (Ll.operand list))), _, (o : (Ll.operand))) = _menhir_stack in
        let _3 = () in
        let _2 = () in
        let _v : (Ll.operand list) =     ( o::os ) in
        _menhir_goto_i_operand_list_rev _menhir_env _menhir_stack _v
    | MenhirState105 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let ((_menhir_stack, (os : (Ll.operand list))), _, (o : (Ll.operand))) = _menhir_stack in
        let _3 = () in
        let _2 = () in
        let _v : (Ll.operand list) =     ( o::os ) in
        _menhir_goto_i_operand_list_rev _menhir_env _menhir_stack _v
    | MenhirState109 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | LPAREN ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | I1 ->
                _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState111
            | I64 ->
                _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState111
            | I8 ->
                _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState111
            | LBRACE ->
                _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState111
            | LBRACKET ->
                _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState111
            | UID _v ->
                _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState111 _v
            | VOID ->
                _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState111
            | COMMA | RPAREN ->
                _menhir_reduce86 _menhir_env (Obj.magic _menhir_stack) MenhirState111
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState111)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState114 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let (((_menhir_stack, _menhir_s, (tos : ((Ll.ty * Ll.operand) list))), _, (t : (Ll.ty))), _, (o : (Ll.operand))) = _menhir_stack in
        let _2 = () in
        let _v : ((Ll.ty * Ll.operand) list) =     ( (t,o)::tos ) in
        _menhir_goto_ty_operand_list_rev _menhir_env _menhir_stack _menhir_s _v
    | MenhirState118 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let ((_menhir_stack, _menhir_s, (t : (Ll.ty))), _, (o : (Ll.operand))) = _menhir_stack in
        let _v : ((Ll.ty * Ll.operand) list) =     ( [(t,o)] ) in
        _menhir_goto_ty_operand_list_rev _menhir_env _menhir_stack _menhir_s _v
    | MenhirState121 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | TO ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | I1 ->
                _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState123
            | I64 ->
                _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState123
            | I8 ->
                _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState123
            | LBRACE ->
                _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState123
            | LBRACKET ->
                _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState123
            | UID _v ->
                _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState123 _v
            | VOID ->
                _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState123
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState123)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState131 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | COMMA ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | GID _v ->
                _menhir_run77 _menhir_env (Obj.magic _menhir_stack) MenhirState133 _v
            | INT _v ->
                _menhir_run76 _menhir_env (Obj.magic _menhir_stack) MenhirState133 _v
            | NULL ->
                _menhir_run75 _menhir_env (Obj.magic _menhir_stack) MenhirState133
            | UID _v ->
                _menhir_run74 _menhir_env (Obj.magic _menhir_stack) MenhirState133 _v
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState133)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState133 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let (((((_menhir_stack, _menhir_s, (u : (string))), (b : (Ll.bop))), _, (t : (Ll.ty))), _, (o1 : (Ll.operand))), _, (o2 : (Ll.operand))) = _menhir_stack in
        let _6 = () in
        let _2 = () in
        let _v : (Ll.uid * Ll.insn) =     ( (u, Binop (b,t,o1,o2)) ) in
        _menhir_goto_insn _menhir_env _menhir_stack _menhir_s _v
    | MenhirState136 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | COMMA ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | I1 ->
                _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState138
            | I64 ->
                _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState138
            | I8 ->
                _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState138
            | LBRACE ->
                _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState138
            | LBRACKET ->
                _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState138
            | UID _v ->
                _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState138 _v
            | VOID ->
                _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState138
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState138)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState139 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let (((((_menhir_stack, _menhir_s), _, (t1 : (Ll.ty))), _, (o1 : (Ll.operand))), _, (t2 : (Ll.ty))), _, (o2 : (Ll.operand))) = _menhir_stack in
        let _4 = () in
        let _1 = () in
        let _v : (Ll.uid * Ll.insn) =     ( (gensym "store", Store (t1,o1,o2)) ) in
        _menhir_goto_insn _menhir_env _menhir_stack _menhir_s _v
    | MenhirState144 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | LPAREN ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | I1 ->
                _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState146
            | I64 ->
                _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState146
            | I8 ->
                _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState146
            | LBRACE ->
                _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState146
            | LBRACKET ->
                _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState146
            | UID _v ->
                _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState146 _v
            | VOID ->
                _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState146
            | COMMA | RPAREN ->
                _menhir_reduce86 _menhir_env (Obj.magic _menhir_stack) MenhirState146
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState146)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState152 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let ((_menhir_stack, _, (t : (Ll.ty))), _, (o : (Ll.operand))) = _menhir_stack in
        let _1 = () in
        let _v : (Ll.terminator) =     ( Ret (t, Some o) ) in
        _menhir_goto_terminator _menhir_env _menhir_stack _v
    | MenhirState157 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | COMMA ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | LABEL ->
                let _menhir_stack = Obj.magic _menhir_stack in
                let _menhir_env = _menhir_discard _menhir_env in
                let _tok = _menhir_env._menhir_token in
                (match _tok with
                | UID _v ->
                    let _menhir_stack = Obj.magic _menhir_stack in
                    let _menhir_stack = (_menhir_stack, _v) in
                    let _menhir_env = _menhir_discard _menhir_env in
                    let _tok = _menhir_env._menhir_token in
                    (match _tok with
                    | COMMA ->
                        let _menhir_stack = Obj.magic _menhir_stack in
                        let _menhir_env = _menhir_discard _menhir_env in
                        let _tok = _menhir_env._menhir_token in
                        (match _tok with
                        | LABEL ->
                            let _menhir_stack = Obj.magic _menhir_stack in
                            let _menhir_env = _menhir_discard _menhir_env in
                            let _tok = _menhir_env._menhir_token in
                            (match _tok with
                            | UID _v ->
                                let _menhir_stack = Obj.magic _menhir_stack in
                                let _menhir_env = _menhir_discard _menhir_env in
                                let _menhir_stack = Obj.magic _menhir_stack in
                                let (l2 : (string)) = _v in
                                let ((_menhir_stack, _, (o : (Ll.operand))), (l1 : (string))) = _menhir_stack in
                                let _8 = () in
                                let _7 = () in
                                let _5 = () in
                                let _4 = () in
                                let _2 = () in
                                let _1 = () in
                                let _v : (Ll.terminator) =     ( Cbr (o,l1,l2) ) in
                                _menhir_goto_terminator _menhir_env _menhir_stack _v
                            | _ ->
                                assert (not _menhir_env._menhir_error);
                                _menhir_env._menhir_error <- true;
                                let _menhir_stack = Obj.magic _menhir_stack in
                                let ((_menhir_stack, _menhir_s, _), _) = _menhir_stack in
                                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
                        | _ ->
                            assert (not _menhir_env._menhir_error);
                            _menhir_env._menhir_error <- true;
                            let _menhir_stack = Obj.magic _menhir_stack in
                            let ((_menhir_stack, _menhir_s, _), _) = _menhir_stack in
                            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
                    | _ ->
                        assert (not _menhir_env._menhir_error);
                        _menhir_env._menhir_error <- true;
                        let _menhir_stack = Obj.magic _menhir_stack in
                        let ((_menhir_stack, _menhir_s, _), _) = _menhir_stack in
                        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
                | _ ->
                    assert (not _menhir_env._menhir_error);
                    _menhir_env._menhir_error <- true;
                    let _menhir_stack = Obj.magic _menhir_stack in
                    let (_menhir_stack, _menhir_s, _) = _menhir_stack in
                    _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                let _menhir_stack = Obj.magic _menhir_stack in
                let (_menhir_stack, _menhir_s, _) = _menhir_stack in
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | _ ->
        _menhir_fail ()

and _menhir_reduce60 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _v : ((Ll.uid * Ll.insn) list) =     ( [] ) in
    _menhir_goto_list_insn_ _menhir_env _menhir_stack _menhir_s _v

and _menhir_run62 : _menhir_env -> 'ttv_tail -> _menhir_state -> (string) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | EQUALS ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_env = _menhir_discard _menhir_env in
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | ADD ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _menhir_stack = Obj.magic _menhir_stack in
            let _1 = () in
            let _v : (Ll.bop) =         ( Add ) in
            _menhir_goto_bop _menhir_env _menhir_stack _v
        | ALLOCA ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | I1 ->
                _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState127
            | I64 ->
                _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState127
            | I8 ->
                _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState127
            | LBRACE ->
                _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState127
            | LBRACKET ->
                _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState127
            | UID _v ->
                _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState127 _v
            | VOID ->
                _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState127
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState127)
        | AND ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _menhir_stack = Obj.magic _menhir_stack in
            let _1 = () in
            let _v : (Ll.bop) =         ( And ) in
            _menhir_goto_bop _menhir_env _menhir_stack _v
        | ASHR ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _menhir_stack = Obj.magic _menhir_stack in
            let _1 = () in
            let _v : (Ll.bop) =          ( Ashr ) in
            _menhir_goto_bop _menhir_env _menhir_stack _v
        | BITCAST ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | I1 ->
                _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState120
            | I64 ->
                _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState120
            | I8 ->
                _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState120
            | LBRACE ->
                _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState120
            | LBRACKET ->
                _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState120
            | UID _v ->
                _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState120 _v
            | VOID ->
                _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState120
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState120)
        | CALL ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | I1 ->
                _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState108
            | I64 ->
                _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState108
            | I8 ->
                _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState108
            | LBRACE ->
                _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState108
            | LBRACKET ->
                _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState108
            | UID _v ->
                _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState108 _v
            | VOID ->
                _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState108
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState108)
        | GEP ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | I1 ->
                _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState91
            | I64 ->
                _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState91
            | I8 ->
                _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState91
            | LBRACE ->
                _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState91
            | LBRACKET ->
                _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState91
            | UID _v ->
                _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState91 _v
            | VOID ->
                _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState91
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState91)
        | ICMP ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | EQ ->
                let _menhir_stack = Obj.magic _menhir_stack in
                let _menhir_env = _menhir_discard _menhir_env in
                let _menhir_stack = Obj.magic _menhir_stack in
                let _1 = () in
                let _v : (Ll.cnd) =        ( Eq ) in
                _menhir_goto_cnd _menhir_env _menhir_stack _v
            | NE ->
                let _menhir_stack = Obj.magic _menhir_stack in
                let _menhir_env = _menhir_discard _menhir_env in
                let _menhir_stack = Obj.magic _menhir_stack in
                let _1 = () in
                let _v : (Ll.cnd) =        ( Ne ) in
                _menhir_goto_cnd _menhir_env _menhir_stack _v
            | SGE ->
                let _menhir_stack = Obj.magic _menhir_stack in
                let _menhir_env = _menhir_discard _menhir_env in
                let _menhir_stack = Obj.magic _menhir_stack in
                let _1 = () in
                let _v : (Ll.cnd) =         ( Sge ) in
                _menhir_goto_cnd _menhir_env _menhir_stack _v
            | SGT ->
                let _menhir_stack = Obj.magic _menhir_stack in
                let _menhir_env = _menhir_discard _menhir_env in
                let _menhir_stack = Obj.magic _menhir_stack in
                let _1 = () in
                let _v : (Ll.cnd) =         ( Sgt ) in
                _menhir_goto_cnd _menhir_env _menhir_stack _v
            | SLE ->
                let _menhir_stack = Obj.magic _menhir_stack in
                let _menhir_env = _menhir_discard _menhir_env in
                let _menhir_stack = Obj.magic _menhir_stack in
                let _1 = () in
                let _v : (Ll.cnd) =         ( Sle ) in
                _menhir_goto_cnd _menhir_env _menhir_stack _v
            | SLT ->
                let _menhir_stack = Obj.magic _menhir_stack in
                let _menhir_env = _menhir_discard _menhir_env in
                let _menhir_stack = Obj.magic _menhir_stack in
                let _1 = () in
                let _v : (Ll.cnd) =         ( Slt ) in
                _menhir_goto_cnd _menhir_env _menhir_stack _v
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                let _menhir_stack = Obj.magic _menhir_stack in
                let (_menhir_stack, _menhir_s, _) = _menhir_stack in
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
        | LOAD ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | I1 ->
                _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState70
            | I64 ->
                _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState70
            | I8 ->
                _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState70
            | LBRACE ->
                _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState70
            | LBRACKET ->
                _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState70
            | UID _v ->
                _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState70 _v
            | VOID ->
                _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState70
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState70)
        | LSHR ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _menhir_stack = Obj.magic _menhir_stack in
            let _1 = () in
            let _v : (Ll.bop) =          ( Lshr ) in
            _menhir_goto_bop _menhir_env _menhir_stack _v
        | MUL ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _menhir_stack = Obj.magic _menhir_stack in
            let _1 = () in
            let _v : (Ll.bop) =         ( Mul ) in
            _menhir_goto_bop _menhir_env _menhir_stack _v
        | OR ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _menhir_stack = Obj.magic _menhir_stack in
            let _1 = () in
            let _v : (Ll.bop) =        ( Or ) in
            _menhir_goto_bop _menhir_env _menhir_stack _v
        | SHL ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _menhir_stack = Obj.magic _menhir_stack in
            let _1 = () in
            let _v : (Ll.bop) =         ( Shl ) in
            _menhir_goto_bop _menhir_env _menhir_stack _v
        | SUB ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _menhir_stack = Obj.magic _menhir_stack in
            let _1 = () in
            let _v : (Ll.bop) =         ( Sub ) in
            _menhir_goto_bop _menhir_env _menhir_stack _v
        | XOR ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _menhir_stack = Obj.magic _menhir_stack in
            let _1 = () in
            let _v : (Ll.bop) =         ( Xor ) in
            _menhir_goto_bop _menhir_env _menhir_stack _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s

and _menhir_run135 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | I1 ->
        _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState135
    | I64 ->
        _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState135
    | I8 ->
        _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState135
    | LBRACE ->
        _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState135
    | LBRACKET ->
        _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState135
    | UID _v ->
        _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState135 _v
    | VOID ->
        _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState135
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState135

and _menhir_run143 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | I1 ->
        _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState143
    | I64 ->
        _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState143
    | I8 ->
        _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState143
    | LBRACE ->
        _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState143
    | LBRACKET ->
        _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState143
    | UID _v ->
        _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState143 _v
    | VOID ->
        _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState143
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState143

and _menhir_reduce35 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _v : (Ll.gdecl list) =     ( [] ) in
    _menhir_goto_gdecl_list_rev _menhir_env _menhir_stack _menhir_s _v

and _menhir_goto_ginit : _menhir_env -> 'ttv_tail -> _menhir_state -> (Ll.ginit) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    match _menhir_s with
    | MenhirState40 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let (g : (Ll.ginit)) = _v in
        let ((_menhir_stack, _menhir_s, (gs : (Ll.gdecl list))), _, (t : (Ll.ty))) = _menhir_stack in
        let _2 = () in
        let _v : (Ll.gdecl list) =     ( (t,g)::gs ) in
        _menhir_goto_gdecl_list_rev _menhir_env _menhir_stack _menhir_s _v
    | MenhirState36 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let (g : (Ll.ginit)) = _v in
        let (_menhir_stack, _menhir_s, (t : (Ll.ty))) = _menhir_stack in
        let _v : (Ll.gdecl list) =     ( [(t,g)] ) in
        _menhir_goto_gdecl_list_rev _menhir_env _menhir_stack _menhir_s _v
    | MenhirState32 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let (gi : (Ll.ginit)) = _v in
        let ((_menhir_stack, (g : (string))), _, (t : (Ll.ty))) = _menhir_stack in
        let _3 = () in
        let _2 = () in
        let _v : (Ll.gid * Ll.gdecl) =     ( (g, (t,gi)) ) in
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let (g : (Ll.gid * Ll.gdecl)) = _v in
        let (_menhir_stack, (ds : (Ll.prog))) = _menhir_stack in
        let _v : (Ll.prog) =     ( { ds with gdecls = g :: ds.gdecls }  ) in
        _menhir_goto_decls_rev _menhir_env _menhir_stack _v
    | _ ->
        _menhir_fail ()

and _menhir_goto_terminator : _menhir_env -> 'ttv_tail -> (Ll.terminator) -> 'ttv_return =
  fun _menhir_env _menhir_stack _v ->
    let _menhir_stack = Obj.magic _menhir_stack in
    let _menhir_stack = Obj.magic _menhir_stack in
    let (t : (Ll.terminator)) = _v in
    let (_menhir_stack, _menhir_s, (is : ((Ll.uid * Ll.insn) list))) = _menhir_stack in
    let _v : (Ll.block) =     ( { insns = is; terminator=t }  ) in
    match _menhir_s with
    | MenhirState142 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let (b : (Ll.block)) = _v in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        let _2 = () in
        let _1 = () in
        let _v : (Ll.block) =     ( b ) in
        _menhir_goto_entry_block _menhir_env _menhir_stack _menhir_s _v
    | MenhirState172 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let (b : (Ll.block)) = _v in
        let ((_menhir_stack, (bs : ((Ll.lbl * Ll.block) list))), (l : (string))) = _menhir_stack in
        let _3 = () in
        let _v : ((Ll.lbl * Ll.block) list) =     ( (l,b) :: bs ) in
        _menhir_goto_block_list_rev _menhir_env _menhir_stack _v
    | MenhirState61 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let (b : (Ll.block)) = _v in
        let _v : (Ll.block) =     ( b ) in
        _menhir_goto_entry_block _menhir_env _menhir_stack _menhir_s _v
    | _ ->
        _menhir_fail ()

and _menhir_goto_insn : _menhir_env -> 'ttv_tail -> _menhir_state -> (Ll.uid * Ll.insn) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    let _menhir_stack = Obj.magic _menhir_stack in
    assert (not _menhir_env._menhir_error);
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | CALL ->
        _menhir_run143 _menhir_env (Obj.magic _menhir_stack) MenhirState166
    | STORE ->
        _menhir_run135 _menhir_env (Obj.magic _menhir_stack) MenhirState166
    | UID _v ->
        _menhir_run62 _menhir_env (Obj.magic _menhir_stack) MenhirState166 _v
    | BR | RET ->
        _menhir_reduce60 _menhir_env (Obj.magic _menhir_stack) MenhirState166
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState166

and _menhir_run74 : _menhir_env -> 'ttv_tail -> _menhir_state -> (string) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _menhir_stack = Obj.magic _menhir_stack in
    let (u : (string)) = _v in
    let _v : (Ll.operand) =     ( Id u ) in
    _menhir_goto_operand _menhir_env _menhir_stack _menhir_s _v

and _menhir_run75 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _menhir_stack = Obj.magic _menhir_stack in
    let _1 = () in
    let _v : (Ll.operand) =     ( Null ) in
    _menhir_goto_operand _menhir_env _menhir_stack _menhir_s _v

and _menhir_run76 : _menhir_env -> 'ttv_tail -> _menhir_state -> (int) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _menhir_stack = Obj.magic _menhir_stack in
    let (i : (int)) = _v in
    let _v : (Ll.operand) =     ( Const (Int64.of_int i) ) in
    _menhir_goto_operand _menhir_env _menhir_stack _menhir_s _v

and _menhir_run77 : _menhir_env -> 'ttv_tail -> _menhir_state -> (string) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _menhir_stack = Obj.magic _menhir_stack in
    let (g : (string)) = _v in
    let _v : (Ll.operand) =     ( Gid g ) in
    _menhir_goto_operand _menhir_env _menhir_stack _menhir_s _v

and _menhir_fail : unit -> 'a =
  fun () ->
    Printf.fprintf Pervasives.stderr "Internal failure -- please contact the parser generator's developers.\n%!";
    assert false

and _menhir_goto_arg_list_rev : _menhir_env -> 'ttv_tail -> _menhir_state -> ((Ll.ty * Ll.uid) list) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    let _menhir_stack = Obj.magic _menhir_stack in
    assert (not _menhir_env._menhir_error);
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | COMMA ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_env = _menhir_discard _menhir_env in
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | I1 ->
            _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState57
        | I64 ->
            _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState57
        | I8 ->
            _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState57
        | LBRACE ->
            _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState57
        | LBRACKET ->
            _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState57
        | UID _v ->
            _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState57 _v
        | VOID ->
            _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState57
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState57)
    | RPAREN ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, (a : ((Ll.ty * Ll.uid) list))) = _menhir_stack in
        let _v : ((Ll.ty * Ll.uid) list) =     ( List.rev a ) in
        let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | RPAREN ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | LBRACE ->
                let _menhir_stack = Obj.magic _menhir_stack in
                let _menhir_env = _menhir_discard _menhir_env in
                let _tok = _menhir_env._menhir_token in
                (match _tok with
                | CALL ->
                    _menhir_run143 _menhir_env (Obj.magic _menhir_stack) MenhirState61
                | ENTRY ->
                    let _menhir_stack = Obj.magic _menhir_stack in
                    let _menhir_s = MenhirState61 in
                    let _menhir_stack = (_menhir_stack, _menhir_s) in
                    let _menhir_env = _menhir_discard _menhir_env in
                    let _tok = _menhir_env._menhir_token in
                    (match _tok with
                    | COLON ->
                        let _menhir_stack = Obj.magic _menhir_stack in
                        let _menhir_env = _menhir_discard _menhir_env in
                        let _tok = _menhir_env._menhir_token in
                        (match _tok with
                        | CALL ->
                            _menhir_run143 _menhir_env (Obj.magic _menhir_stack) MenhirState142
                        | STORE ->
                            _menhir_run135 _menhir_env (Obj.magic _menhir_stack) MenhirState142
                        | UID _v ->
                            _menhir_run62 _menhir_env (Obj.magic _menhir_stack) MenhirState142 _v
                        | BR | RET ->
                            _menhir_reduce60 _menhir_env (Obj.magic _menhir_stack) MenhirState142
                        | _ ->
                            assert (not _menhir_env._menhir_error);
                            _menhir_env._menhir_error <- true;
                            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState142)
                    | _ ->
                        assert (not _menhir_env._menhir_error);
                        _menhir_env._menhir_error <- true;
                        let _menhir_stack = Obj.magic _menhir_stack in
                        let (_menhir_stack, _menhir_s) = _menhir_stack in
                        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
                | STORE ->
                    _menhir_run135 _menhir_env (Obj.magic _menhir_stack) MenhirState61
                | UID _v ->
                    _menhir_run62 _menhir_env (Obj.magic _menhir_stack) MenhirState61 _v
                | BR | RET ->
                    _menhir_reduce60 _menhir_env (Obj.magic _menhir_stack) MenhirState61
                | _ ->
                    assert (not _menhir_env._menhir_error);
                    _menhir_env._menhir_error <- true;
                    _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState61)
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                let _menhir_stack = Obj.magic _menhir_stack in
                let (_menhir_stack, _menhir_s, _) = _menhir_stack in
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s

and _menhir_run33 : _menhir_env -> 'ttv_tail -> _menhir_state -> (string) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _menhir_stack = Obj.magic _menhir_stack in
    let (s : (string)) = _v in
    let _v : (Ll.ginit) =     ( GString s ) in
    _menhir_goto_ginit _menhir_env _menhir_stack _menhir_s _v

and _menhir_run34 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _menhir_stack = Obj.magic _menhir_stack in
    let _1 = () in
    let _v : (Ll.ginit) =     ( GNull ) in
    _menhir_goto_ginit _menhir_env _menhir_stack _menhir_s _v

and _menhir_run35 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | I1 ->
        _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState35
    | I64 ->
        _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState35
    | I8 ->
        _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState35
    | LBRACE ->
        _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState35
    | LBRACKET ->
        _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState35
    | UID _v ->
        _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState35 _v
    | VOID ->
        _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState35
    | COMMA | RBRACKET ->
        _menhir_reduce35 _menhir_env (Obj.magic _menhir_stack) MenhirState35
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState35

and _menhir_run37 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | I1 ->
        _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState37
    | I64 ->
        _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState37
    | I8 ->
        _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState37
    | LBRACE ->
        _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState37
    | LBRACKET ->
        _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState37
    | UID _v ->
        _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState37 _v
    | VOID ->
        _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState37
    | COMMA | RBRACE ->
        _menhir_reduce35 _menhir_env (Obj.magic _menhir_stack) MenhirState37
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState37

and _menhir_run41 : _menhir_env -> 'ttv_tail -> _menhir_state -> (int) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _menhir_stack = Obj.magic _menhir_stack in
    let (i : (int)) = _v in
    let _v : (Ll.ginit) =     ( GInt (Int64.of_int i) ) in
    _menhir_goto_ginit _menhir_env _menhir_stack _menhir_s _v

and _menhir_run42 : _menhir_env -> 'ttv_tail -> _menhir_state -> (string) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _menhir_stack = Obj.magic _menhir_stack in
    let (g : (string)) = _v in
    let _v : (Ll.ginit) =     ( GGid g ) in
    _menhir_goto_ginit _menhir_env _menhir_stack _menhir_s _v

and _menhir_goto_ty_list_rev : _menhir_env -> 'ttv_tail -> _menhir_state -> (Ll.ty list) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    let _menhir_stack = Obj.magic _menhir_stack in
    assert (not _menhir_env._menhir_error);
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | COMMA ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_env = _menhir_discard _menhir_env in
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | I1 ->
            _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState16
        | I64 ->
            _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState16
        | I8 ->
            _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState16
        | LBRACE ->
            _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState16
        | LBRACKET ->
            _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState16
        | UID _v ->
            _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState16 _v
        | VOID ->
            _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState16
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState16)
    | RBRACE | RPAREN ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, (ts : (Ll.ty list))) = _menhir_stack in
        let _v : (Ll.ty list) =     ( List.rev ts ) in
        let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
        (match _menhir_s with
        | MenhirState19 ->
            let _menhir_stack = Obj.magic _menhir_stack in
            assert (not _menhir_env._menhir_error);
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | RPAREN ->
                let _menhir_stack = Obj.magic _menhir_stack in
                let _menhir_env = _menhir_discard _menhir_env in
                let _menhir_stack = Obj.magic _menhir_stack in
                let (((_menhir_stack, _menhir_s, (rt : (Ll.ty))), _), _, (ts : (Ll.ty list))) = _menhir_stack in
                let _4 = () in
                let _2 = () in
                let _v : (Ll.ty) =     ( Fun (ts, rt) ) in
                _menhir_goto_nonptr_ty _menhir_env _menhir_stack _menhir_s _v
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                let _menhir_stack = Obj.magic _menhir_stack in
                let (_menhir_stack, _menhir_s, _) = _menhir_stack in
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
        | MenhirState11 ->
            let _menhir_stack = Obj.magic _menhir_stack in
            assert (not _menhir_env._menhir_error);
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | RBRACE ->
                let _menhir_stack = Obj.magic _menhir_stack in
                let _menhir_env = _menhir_discard _menhir_env in
                let _menhir_stack = Obj.magic _menhir_stack in
                let ((_menhir_stack, _menhir_s), _, (ts : (Ll.ty list))) = _menhir_stack in
                let _3 = () in
                let _1 = () in
                let _v : (Ll.ty) =     ( Struct ts ) in
                _menhir_goto_nonptr_ty _menhir_env _menhir_stack _menhir_s _v
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                let _menhir_stack = Obj.magic _menhir_stack in
                let (_menhir_stack, _menhir_s, _) = _menhir_stack in
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
        | _ ->
            _menhir_fail ())
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s

and _menhir_run18 : _menhir_env -> 'ttv_tail * _menhir_state * (Ll.ty) -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _menhir_stack = Obj.magic _menhir_stack in
    let (_menhir_stack, _menhir_s, (t : (Ll.ty))) = _menhir_stack in
    let _2 = () in
    let _v : (Ll.ty) =     ( Ptr t ) in
    _menhir_goto_ty _menhir_env _menhir_stack _menhir_s _v

and _menhir_run19 : _menhir_env -> 'ttv_tail * _menhir_state * (Ll.ty) -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | I1 ->
        _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState19
    | I64 ->
        _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState19
    | I8 ->
        _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState19
    | LBRACE ->
        _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState19
    | LBRACKET ->
        _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState19
    | UID _v ->
        _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState19 _v
    | VOID ->
        _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState19
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState19

and _menhir_goto_ty : _menhir_env -> 'ttv_tail -> _menhir_state -> (Ll.ty) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    match _menhir_s with
    | MenhirState16 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | LPAREN ->
            _menhir_run19 _menhir_env (Obj.magic _menhir_stack) MenhirState17
        | STAR ->
            _menhir_run18 _menhir_env (Obj.magic _menhir_stack) MenhirState17
        | COMMA | RBRACE | RPAREN ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s, (ts : (Ll.ty list))), _, (t : (Ll.ty))) = _menhir_stack in
            let _2 = () in
            let _v : (Ll.ty list) =     ( t::ts ) in
            _menhir_goto_ty_list_rev _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState17)
    | MenhirState11 | MenhirState19 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | LPAREN ->
            _menhir_run19 _menhir_env (Obj.magic _menhir_stack) MenhirState22
        | STAR ->
            _menhir_run18 _menhir_env (Obj.magic _menhir_stack) MenhirState22
        | COMMA | RBRACE | RPAREN ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, (t : (Ll.ty))) = _menhir_stack in
            let _v : (Ll.ty list) =     ( [t] ) in
            _menhir_goto_ty_list_rev _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState22)
    | MenhirState10 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | LPAREN ->
            _menhir_run19 _menhir_env (Obj.magic _menhir_stack) MenhirState26
        | RBRACKET ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_s = MenhirState26 in
            let _menhir_env = _menhir_discard _menhir_env in
            let _menhir_stack = Obj.magic _menhir_stack in
            let (((_menhir_stack, _menhir_s), (i : (int))), _, (t : (Ll.ty))) = _menhir_stack in
            let _5 = () in
            let _3 = () in
            let _1 = () in
            let _v : (Ll.ty) =     ( Array (i,t) ) in
            _menhir_goto_nonptr_ty _menhir_env _menhir_stack _menhir_s _v
        | STAR ->
            _menhir_run18 _menhir_env (Obj.magic _menhir_stack) MenhirState26
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState26)
    | MenhirState5 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | LPAREN ->
            _menhir_run19 _menhir_env (Obj.magic _menhir_stack) MenhirState28
        | STAR ->
            _menhir_run18 _menhir_env (Obj.magic _menhir_stack) MenhirState28
        | DEFINE | EOF | GID _ | UID _ ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, (tid : (string))), _, (t : (Ll.ty))) = _menhir_stack in
            let _3 = () in
            let _2 = () in
            let _v : (Ll.tid * Ll.ty) =     ( (tid, t) ) in
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_stack = Obj.magic _menhir_stack in
            let (t : (Ll.tid * Ll.ty)) = _v in
            let (_menhir_stack, (ds : (Ll.prog))) = _menhir_stack in
            let _v : (Ll.prog) =     ( { ds with tdecls = t :: ds.tdecls }  ) in
            _menhir_goto_decls_rev _menhir_env _menhir_stack _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState28)
    | MenhirState31 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | GID _v ->
            _menhir_run42 _menhir_env (Obj.magic _menhir_stack) MenhirState32 _v
        | INT _v ->
            _menhir_run41 _menhir_env (Obj.magic _menhir_stack) MenhirState32 _v
        | LBRACE ->
            _menhir_run37 _menhir_env (Obj.magic _menhir_stack) MenhirState32
        | LBRACKET ->
            _menhir_run35 _menhir_env (Obj.magic _menhir_stack) MenhirState32
        | LPAREN ->
            _menhir_run19 _menhir_env (Obj.magic _menhir_stack) MenhirState32
        | NULL ->
            _menhir_run34 _menhir_env (Obj.magic _menhir_stack) MenhirState32
        | STAR ->
            _menhir_run18 _menhir_env (Obj.magic _menhir_stack) MenhirState32
        | STRING _v ->
            _menhir_run33 _menhir_env (Obj.magic _menhir_stack) MenhirState32 _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState32)
    | MenhirState37 | MenhirState35 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | GID _v ->
            _menhir_run42 _menhir_env (Obj.magic _menhir_stack) MenhirState36 _v
        | INT _v ->
            _menhir_run41 _menhir_env (Obj.magic _menhir_stack) MenhirState36 _v
        | LBRACE ->
            _menhir_run37 _menhir_env (Obj.magic _menhir_stack) MenhirState36
        | LBRACKET ->
            _menhir_run35 _menhir_env (Obj.magic _menhir_stack) MenhirState36
        | LPAREN ->
            _menhir_run19 _menhir_env (Obj.magic _menhir_stack) MenhirState36
        | NULL ->
            _menhir_run34 _menhir_env (Obj.magic _menhir_stack) MenhirState36
        | STAR ->
            _menhir_run18 _menhir_env (Obj.magic _menhir_stack) MenhirState36
        | STRING _v ->
            _menhir_run33 _menhir_env (Obj.magic _menhir_stack) MenhirState36 _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState36)
    | MenhirState39 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | GID _v ->
            _menhir_run42 _menhir_env (Obj.magic _menhir_stack) MenhirState40 _v
        | INT _v ->
            _menhir_run41 _menhir_env (Obj.magic _menhir_stack) MenhirState40 _v
        | LBRACE ->
            _menhir_run37 _menhir_env (Obj.magic _menhir_stack) MenhirState40
        | LBRACKET ->
            _menhir_run35 _menhir_env (Obj.magic _menhir_stack) MenhirState40
        | LPAREN ->
            _menhir_run19 _menhir_env (Obj.magic _menhir_stack) MenhirState40
        | NULL ->
            _menhir_run34 _menhir_env (Obj.magic _menhir_stack) MenhirState40
        | STAR ->
            _menhir_run18 _menhir_env (Obj.magic _menhir_stack) MenhirState40
        | STRING _v ->
            _menhir_run33 _menhir_env (Obj.magic _menhir_stack) MenhirState40 _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState40)
    | MenhirState50 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | GID _v ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_s = MenhirState51 in
            let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | LPAREN ->
                let _menhir_stack = Obj.magic _menhir_stack in
                let _menhir_env = _menhir_discard _menhir_env in
                let _tok = _menhir_env._menhir_token in
                (match _tok with
                | I1 ->
                    _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState53
                | I64 ->
                    _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState53
                | I8 ->
                    _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState53
                | LBRACE ->
                    _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState53
                | LBRACKET ->
                    _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState53
                | UID _v ->
                    _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState53 _v
                | VOID ->
                    _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState53
                | COMMA | RPAREN ->
                    let _menhir_stack = Obj.magic _menhir_stack in
                    let _menhir_s = MenhirState53 in
                    let _v : ((Ll.ty * Ll.uid) list) =     ( [] ) in
                    _menhir_goto_arg_list_rev _menhir_env _menhir_stack _menhir_s _v
                | _ ->
                    assert (not _menhir_env._menhir_error);
                    _menhir_env._menhir_error <- true;
                    _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState53)
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                let _menhir_stack = Obj.magic _menhir_stack in
                let (_menhir_stack, _menhir_s, _) = _menhir_stack in
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
        | LPAREN ->
            _menhir_run19 _menhir_env (Obj.magic _menhir_stack) MenhirState51
        | STAR ->
            _menhir_run18 _menhir_env (Obj.magic _menhir_stack) MenhirState51
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState51)
    | MenhirState57 | MenhirState53 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | LPAREN ->
            _menhir_run19 _menhir_env (Obj.magic _menhir_stack) MenhirState54
        | STAR ->
            _menhir_run18 _menhir_env (Obj.magic _menhir_stack) MenhirState54
        | UID _v ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_s = MenhirState54 in
            let _menhir_env = _menhir_discard _menhir_env in
            let _menhir_stack = Obj.magic _menhir_stack in
            let (u : (string)) = _v in
            let (_menhir_stack, _menhir_s, (t : (Ll.ty))) = _menhir_stack in
            let _v : (Ll.ty * Ll.uid) =                ( (t,u) ) in
            (match _menhir_s with
            | MenhirState57 ->
                let _menhir_stack = Obj.magic _menhir_stack in
                let _menhir_stack = Obj.magic _menhir_stack in
                let (a : (Ll.ty * Ll.uid)) = _v in
                let (_menhir_stack, _menhir_s, (args : ((Ll.ty * Ll.uid) list))) = _menhir_stack in
                let _2 = () in
                let _v : ((Ll.ty * Ll.uid) list) =     ( a::args ) in
                _menhir_goto_arg_list_rev _menhir_env _menhir_stack _menhir_s _v
            | MenhirState53 ->
                let _menhir_stack = Obj.magic _menhir_stack in
                let _menhir_stack = Obj.magic _menhir_stack in
                let (a : (Ll.ty * Ll.uid)) = _v in
                let _v : ((Ll.ty * Ll.uid) list) =     ( [a] ) in
                _menhir_goto_arg_list_rev _menhir_env _menhir_stack _menhir_s _v
            | _ ->
                _menhir_fail ())
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState54)
    | MenhirState70 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | COMMA ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_s = MenhirState71 in
            let _menhir_stack = (_menhir_stack, _menhir_s) in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | I1 ->
                _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState72
            | I64 ->
                _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState72
            | I8 ->
                _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState72
            | LBRACE ->
                _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState72
            | LBRACKET ->
                _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState72
            | UID _v ->
                _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState72 _v
            | VOID ->
                _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState72
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState72)
        | LPAREN ->
            _menhir_run19 _menhir_env (Obj.magic _menhir_stack) MenhirState71
        | STAR ->
            _menhir_run18 _menhir_env (Obj.magic _menhir_stack) MenhirState71
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState71)
    | MenhirState72 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | GID _v ->
            _menhir_run77 _menhir_env (Obj.magic _menhir_stack) MenhirState73 _v
        | INT _v ->
            _menhir_run76 _menhir_env (Obj.magic _menhir_stack) MenhirState73 _v
        | LPAREN ->
            _menhir_run19 _menhir_env (Obj.magic _menhir_stack) MenhirState73
        | NULL ->
            _menhir_run75 _menhir_env (Obj.magic _menhir_stack) MenhirState73
        | STAR ->
            _menhir_run18 _menhir_env (Obj.magic _menhir_stack) MenhirState73
        | UID _v ->
            _menhir_run74 _menhir_env (Obj.magic _menhir_stack) MenhirState73 _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState73)
    | MenhirState86 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | GID _v ->
            _menhir_run77 _menhir_env (Obj.magic _menhir_stack) MenhirState87 _v
        | INT _v ->
            _menhir_run76 _menhir_env (Obj.magic _menhir_stack) MenhirState87 _v
        | LPAREN ->
            _menhir_run19 _menhir_env (Obj.magic _menhir_stack) MenhirState87
        | NULL ->
            _menhir_run75 _menhir_env (Obj.magic _menhir_stack) MenhirState87
        | STAR ->
            _menhir_run18 _menhir_env (Obj.magic _menhir_stack) MenhirState87
        | UID _v ->
            _menhir_run74 _menhir_env (Obj.magic _menhir_stack) MenhirState87 _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState87)
    | MenhirState91 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | COMMA ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_s = MenhirState92 in
            let _menhir_stack = (_menhir_stack, _menhir_s) in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | I1 ->
                _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState93
            | I64 ->
                _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState93
            | I8 ->
                _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState93
            | LBRACE ->
                _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState93
            | LBRACKET ->
                _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState93
            | UID _v ->
                _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState93 _v
            | VOID ->
                _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState93
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState93)
        | LPAREN ->
            _menhir_run19 _menhir_env (Obj.magic _menhir_stack) MenhirState92
        | STAR ->
            _menhir_run18 _menhir_env (Obj.magic _menhir_stack) MenhirState92
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState92)
    | MenhirState93 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | GID _v ->
            _menhir_run77 _menhir_env (Obj.magic _menhir_stack) MenhirState94 _v
        | INT _v ->
            _menhir_run76 _menhir_env (Obj.magic _menhir_stack) MenhirState94 _v
        | LPAREN ->
            _menhir_run19 _menhir_env (Obj.magic _menhir_stack) MenhirState94
        | NULL ->
            _menhir_run75 _menhir_env (Obj.magic _menhir_stack) MenhirState94
        | STAR ->
            _menhir_run18 _menhir_env (Obj.magic _menhir_stack) MenhirState94
        | UID _v ->
            _menhir_run74 _menhir_env (Obj.magic _menhir_stack) MenhirState94 _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState94)
    | MenhirState108 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | GID _v ->
            _menhir_run77 _menhir_env (Obj.magic _menhir_stack) MenhirState109 _v
        | INT _v ->
            _menhir_run76 _menhir_env (Obj.magic _menhir_stack) MenhirState109 _v
        | LPAREN ->
            _menhir_run19 _menhir_env (Obj.magic _menhir_stack) MenhirState109
        | NULL ->
            _menhir_run75 _menhir_env (Obj.magic _menhir_stack) MenhirState109
        | STAR ->
            _menhir_run18 _menhir_env (Obj.magic _menhir_stack) MenhirState109
        | UID _v ->
            _menhir_run74 _menhir_env (Obj.magic _menhir_stack) MenhirState109 _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState109)
    | MenhirState113 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | GID _v ->
            _menhir_run77 _menhir_env (Obj.magic _menhir_stack) MenhirState114 _v
        | INT _v ->
            _menhir_run76 _menhir_env (Obj.magic _menhir_stack) MenhirState114 _v
        | LPAREN ->
            _menhir_run19 _menhir_env (Obj.magic _menhir_stack) MenhirState114
        | NULL ->
            _menhir_run75 _menhir_env (Obj.magic _menhir_stack) MenhirState114
        | STAR ->
            _menhir_run18 _menhir_env (Obj.magic _menhir_stack) MenhirState114
        | UID _v ->
            _menhir_run74 _menhir_env (Obj.magic _menhir_stack) MenhirState114 _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState114)
    | MenhirState146 | MenhirState111 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | GID _v ->
            _menhir_run77 _menhir_env (Obj.magic _menhir_stack) MenhirState118 _v
        | INT _v ->
            _menhir_run76 _menhir_env (Obj.magic _menhir_stack) MenhirState118 _v
        | LPAREN ->
            _menhir_run19 _menhir_env (Obj.magic _menhir_stack) MenhirState118
        | NULL ->
            _menhir_run75 _menhir_env (Obj.magic _menhir_stack) MenhirState118
        | STAR ->
            _menhir_run18 _menhir_env (Obj.magic _menhir_stack) MenhirState118
        | UID _v ->
            _menhir_run74 _menhir_env (Obj.magic _menhir_stack) MenhirState118 _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState118)
    | MenhirState120 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | GID _v ->
            _menhir_run77 _menhir_env (Obj.magic _menhir_stack) MenhirState121 _v
        | INT _v ->
            _menhir_run76 _menhir_env (Obj.magic _menhir_stack) MenhirState121 _v
        | LPAREN ->
            _menhir_run19 _menhir_env (Obj.magic _menhir_stack) MenhirState121
        | NULL ->
            _menhir_run75 _menhir_env (Obj.magic _menhir_stack) MenhirState121
        | STAR ->
            _menhir_run18 _menhir_env (Obj.magic _menhir_stack) MenhirState121
        | UID _v ->
            _menhir_run74 _menhir_env (Obj.magic _menhir_stack) MenhirState121 _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState121)
    | MenhirState123 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | LPAREN ->
            _menhir_run19 _menhir_env (Obj.magic _menhir_stack) MenhirState124
        | STAR ->
            _menhir_run18 _menhir_env (Obj.magic _menhir_stack) MenhirState124
        | BR | CALL | RET | STORE | UID _ ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((((_menhir_stack, _menhir_s, (u : (string))), _, (t1 : (Ll.ty))), _, (o : (Ll.operand))), _, (t2 : (Ll.ty))) = _menhir_stack in
            let _6 = () in
            let _3 = () in
            let _2 = () in
            let _v : (Ll.uid * Ll.insn) =     ( (u, Bitcast (t1,o,t2)) ) in
            _menhir_goto_insn _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState124)
    | MenhirState127 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | LPAREN ->
            _menhir_run19 _menhir_env (Obj.magic _menhir_stack) MenhirState128
        | STAR ->
            _menhir_run18 _menhir_env (Obj.magic _menhir_stack) MenhirState128
        | BR | CALL | RET | STORE | UID _ ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s, (u : (string))), _, (t : (Ll.ty))) = _menhir_stack in
            let _3 = () in
            let _2 = () in
            let _v : (Ll.uid * Ll.insn) =     ( (u, Alloca t) ) in
            _menhir_goto_insn _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState128)
    | MenhirState130 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | GID _v ->
            _menhir_run77 _menhir_env (Obj.magic _menhir_stack) MenhirState131 _v
        | INT _v ->
            _menhir_run76 _menhir_env (Obj.magic _menhir_stack) MenhirState131 _v
        | LPAREN ->
            _menhir_run19 _menhir_env (Obj.magic _menhir_stack) MenhirState131
        | NULL ->
            _menhir_run75 _menhir_env (Obj.magic _menhir_stack) MenhirState131
        | STAR ->
            _menhir_run18 _menhir_env (Obj.magic _menhir_stack) MenhirState131
        | UID _v ->
            _menhir_run74 _menhir_env (Obj.magic _menhir_stack) MenhirState131 _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState131)
    | MenhirState135 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | GID _v ->
            _menhir_run77 _menhir_env (Obj.magic _menhir_stack) MenhirState136 _v
        | INT _v ->
            _menhir_run76 _menhir_env (Obj.magic _menhir_stack) MenhirState136 _v
        | LPAREN ->
            _menhir_run19 _menhir_env (Obj.magic _menhir_stack) MenhirState136
        | NULL ->
            _menhir_run75 _menhir_env (Obj.magic _menhir_stack) MenhirState136
        | STAR ->
            _menhir_run18 _menhir_env (Obj.magic _menhir_stack) MenhirState136
        | UID _v ->
            _menhir_run74 _menhir_env (Obj.magic _menhir_stack) MenhirState136 _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState136)
    | MenhirState138 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | GID _v ->
            _menhir_run77 _menhir_env (Obj.magic _menhir_stack) MenhirState139 _v
        | INT _v ->
            _menhir_run76 _menhir_env (Obj.magic _menhir_stack) MenhirState139 _v
        | LPAREN ->
            _menhir_run19 _menhir_env (Obj.magic _menhir_stack) MenhirState139
        | NULL ->
            _menhir_run75 _menhir_env (Obj.magic _menhir_stack) MenhirState139
        | STAR ->
            _menhir_run18 _menhir_env (Obj.magic _menhir_stack) MenhirState139
        | UID _v ->
            _menhir_run74 _menhir_env (Obj.magic _menhir_stack) MenhirState139 _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState139)
    | MenhirState143 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | GID _v ->
            _menhir_run77 _menhir_env (Obj.magic _menhir_stack) MenhirState144 _v
        | INT _v ->
            _menhir_run76 _menhir_env (Obj.magic _menhir_stack) MenhirState144 _v
        | LPAREN ->
            _menhir_run19 _menhir_env (Obj.magic _menhir_stack) MenhirState144
        | NULL ->
            _menhir_run75 _menhir_env (Obj.magic _menhir_stack) MenhirState144
        | STAR ->
            _menhir_run18 _menhir_env (Obj.magic _menhir_stack) MenhirState144
        | UID _v ->
            _menhir_run74 _menhir_env (Obj.magic _menhir_stack) MenhirState144 _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState144)
    | MenhirState151 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | GID _v ->
            _menhir_run77 _menhir_env (Obj.magic _menhir_stack) MenhirState152 _v
        | INT _v ->
            _menhir_run76 _menhir_env (Obj.magic _menhir_stack) MenhirState152 _v
        | LPAREN ->
            _menhir_run19 _menhir_env (Obj.magic _menhir_stack) MenhirState152
        | NULL ->
            _menhir_run75 _menhir_env (Obj.magic _menhir_stack) MenhirState152
        | STAR ->
            _menhir_run18 _menhir_env (Obj.magic _menhir_stack) MenhirState152
        | UID _v ->
            _menhir_run74 _menhir_env (Obj.magic _menhir_stack) MenhirState152 _v
        | LBL _ | RBRACE ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _, (t : (Ll.ty))) = _menhir_stack in
            let _1 = () in
            let _v : (Ll.terminator) =     ( Ret (t, None) ) in
            _menhir_goto_terminator _menhir_env _menhir_stack _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState152)
    | _ ->
        _menhir_fail ()

and _menhir_goto_nonptr_ty : _menhir_env -> 'ttv_tail -> _menhir_state -> (Ll.ty) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = Obj.magic _menhir_stack in
    let _menhir_stack = Obj.magic _menhir_stack in
    let (t : (Ll.ty)) = _v in
    let _v : (Ll.ty) =     ( t ) in
    _menhir_goto_ty _menhir_env _menhir_stack _menhir_s _v

and _menhir_errorcase : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    match _menhir_s with
    | MenhirState172 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        raise _eRR
    | MenhirState166 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState157 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        raise _eRR
    | MenhirState152 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState151 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        raise _eRR
    | MenhirState146 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState144 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState143 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState142 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState139 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState138 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState136 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState135 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState133 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState131 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState130 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let ((_menhir_stack, _menhir_s, _), _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState128 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState127 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState124 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState123 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState121 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState120 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState118 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState114 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState113 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState111 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState109 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState108 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState105 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        raise _eRR
    | MenhirState103 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        raise _eRR
    | MenhirState99 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        raise _eRR
    | MenhirState97 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        raise _eRR
    | MenhirState94 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState93 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState92 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState91 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState89 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState87 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState86 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let ((_menhir_stack, _menhir_s, _), _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState73 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState72 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState71 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState70 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState61 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState57 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState54 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState53 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState51 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState50 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        raise _eRR
    | MenhirState40 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState39 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState37 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState36 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState35 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState32 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState31 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        raise _eRR
    | MenhirState28 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState26 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState22 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState19 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState17 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState16 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState11 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState10 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let ((_menhir_stack, _menhir_s), _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState5 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        raise _eRR

and _menhir_run6 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _menhir_stack = Obj.magic _menhir_stack in
    let _1 = () in
    let _v : (Ll.ty) =          ( Void ) in
    _menhir_goto_nonptr_ty _menhir_env _menhir_stack _menhir_s _v

and _menhir_run7 : _menhir_env -> 'ttv_tail -> _menhir_state -> (string) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _menhir_stack = Obj.magic _menhir_stack in
    let (t : (string)) = _v in
    let _v : (Ll.ty) =     ( Namedt t ) in
    _menhir_goto_nonptr_ty _menhir_env _menhir_stack _menhir_s _v

and _menhir_run8 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | INT _v ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = (_menhir_stack, _v) in
        let _menhir_env = _menhir_discard _menhir_env in
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | CROSS ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | I1 ->
                _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState10
            | I64 ->
                _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState10
            | I8 ->
                _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState10
            | LBRACE ->
                _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState10
            | LBRACKET ->
                _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState10
            | UID _v ->
                _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState10 _v
            | VOID ->
                _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState10
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState10)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s), _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s

and _menhir_run11 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | I1 ->
        _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState11
    | I64 ->
        _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState11
    | I8 ->
        _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState11
    | LBRACE ->
        _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState11
    | LBRACKET ->
        _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState11
    | UID _v ->
        _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState11 _v
    | VOID ->
        _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState11
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState11

and _menhir_run12 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _menhir_stack = Obj.magic _menhir_stack in
    let _1 = () in
    let _v : (Ll.ty) =        ( I8 ) in
    _menhir_goto_nonptr_ty _menhir_env _menhir_stack _menhir_s _v

and _menhir_run13 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _menhir_stack = Obj.magic _menhir_stack in
    let _1 = () in
    let _v : (Ll.ty) =         ( I64 ) in
    _menhir_goto_nonptr_ty _menhir_env _menhir_stack _menhir_s _v

and _menhir_run14 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _menhir_stack = Obj.magic _menhir_stack in
    let _1 = () in
    let _v : (Ll.ty) =        ( I1 ) in
    _menhir_goto_nonptr_ty _menhir_env _menhir_stack _menhir_s _v

and _menhir_goto_decls_rev : _menhir_env -> 'ttv_tail -> (Ll.prog) -> 'ttv_return =
  fun _menhir_env _menhir_stack _v ->
    let _menhir_stack = (_menhir_stack, _v) in
    let _menhir_stack = Obj.magic _menhir_stack in
    assert (not _menhir_env._menhir_error);
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | DEFINE ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_env = _menhir_discard _menhir_env in
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | I1 ->
            _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState50
        | I64 ->
            _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState50
        | I8 ->
            _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState50
        | LBRACE ->
            _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState50
        | LBRACKET ->
            _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState50
        | UID _v ->
            _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState50 _v
        | VOID ->
            _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState50
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState50)
    | GID _v ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = (_menhir_stack, _v) in
        let _menhir_env = _menhir_discard _menhir_env in
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | EQUALS ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | GLOBAL ->
                let _menhir_stack = Obj.magic _menhir_stack in
                let _menhir_env = _menhir_discard _menhir_env in
                let _tok = _menhir_env._menhir_token in
                (match _tok with
                | I1 ->
                    _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState31
                | I64 ->
                    _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState31
                | I8 ->
                    _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState31
                | LBRACE ->
                    _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState31
                | LBRACKET ->
                    _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState31
                | UID _v ->
                    _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState31 _v
                | VOID ->
                    _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState31
                | _ ->
                    assert (not _menhir_env._menhir_error);
                    _menhir_env._menhir_error <- true;
                    _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState31)
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                let _menhir_stack = Obj.magic _menhir_stack in
                raise _eRR)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            raise _eRR)
    | UID _v ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = (_menhir_stack, _v) in
        let _menhir_env = _menhir_discard _menhir_env in
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | EQUALS ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | TYPE ->
                let _menhir_stack = Obj.magic _menhir_stack in
                let _menhir_env = _menhir_discard _menhir_env in
                let _tok = _menhir_env._menhir_token in
                (match _tok with
                | I1 ->
                    _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState5
                | I64 ->
                    _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState5
                | I8 ->
                    _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState5
                | LBRACE ->
                    _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState5
                | LBRACKET ->
                    _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState5
                | UID _v ->
                    _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState5 _v
                | VOID ->
                    _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState5
                | _ ->
                    assert (not _menhir_env._menhir_error);
                    _menhir_env._menhir_error <- true;
                    _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState5)
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                let _menhir_stack = Obj.magic _menhir_stack in
                raise _eRR)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            raise _eRR)
    | EOF ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, (ds : (Ll.prog))) = _menhir_stack in
        let _v : (Ll.prog) =     ( { tdecls = List.rev ds.tdecls
      ; gdecls = List.rev ds.gdecls
      ; fdecls = List.rev ds.fdecls
    } ) in
        let _menhir_stack = (_menhir_stack, _v) in
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | EOF ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, (ds : (Ll.prog))) = _menhir_stack in
            let _2 = () in
            let _v : (Ll.prog) =     ( ds ) in
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_1 : (Ll.prog)) = _v in
            Obj.magic _1
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            raise _eRR)
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        let _menhir_stack = Obj.magic _menhir_stack in
        raise _eRR

and _menhir_discard : _menhir_env -> _menhir_env =
  fun _menhir_env ->
    let lexer = _menhir_env._menhir_lexer in
    let lexbuf = _menhir_env._menhir_lexbuf in
    let _tok = lexer lexbuf in
    {
      _menhir_lexer = lexer;
      _menhir_lexbuf = lexbuf;
      _menhir_token = _tok;
      _menhir_error = false;
    }

and prog : (Lexing.lexbuf -> token) -> Lexing.lexbuf -> (Ll.prog) =
  fun lexer lexbuf ->
    let _menhir_env = let _tok = Obj.magic () in
    {
      _menhir_lexer = lexer;
      _menhir_lexbuf = lexbuf;
      _menhir_token = _tok;
      _menhir_error = false;
    } in
    Obj.magic (let _menhir_stack = ((), _menhir_env._menhir_lexbuf.Lexing.lex_curr_p) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _menhir_stack = Obj.magic _menhir_stack in
    let _v : (Ll.prog) =     ( { tdecls = [] ; gdecls = [] ; fdecls = [] } ) in
    _menhir_goto_decls_rev _menhir_env _menhir_stack _v)
  

