
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
    | EXTERNAL
    | EQUALS
    | EQ
    | EOF
    | ENTRY
    | DEFINE
    | DECLARE
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
  | MenhirState186
  | MenhirState185
  | MenhirState184
  | MenhirState182
  | MenhirState181
  | MenhirState175
  | MenhirState169
  | MenhirState160
  | MenhirState155
  | MenhirState154
  | MenhirState149
  | MenhirState147
  | MenhirState146
  | MenhirState145
  | MenhirState142
  | MenhirState141
  | MenhirState139
  | MenhirState138
  | MenhirState136
  | MenhirState134
  | MenhirState133
  | MenhirState131
  | MenhirState130
  | MenhirState127
  | MenhirState126
  | MenhirState124
  | MenhirState123
  | MenhirState121
  | MenhirState117
  | MenhirState116
  | MenhirState114
  | MenhirState112
  | MenhirState111
  | MenhirState108
  | MenhirState106
  | MenhirState102
  | MenhirState100
  | MenhirState97
  | MenhirState96
  | MenhirState95
  | MenhirState94
  | MenhirState92
  | MenhirState90
  | MenhirState89
  | MenhirState76
  | MenhirState75
  | MenhirState74
  | MenhirState73
  | MenhirState64
  | MenhirState60
  | MenhirState57
  | MenhirState56
  | MenhirState54
  | MenhirState53
  | MenhirState52
  | MenhirState51
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
            _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState116
        | I64 ->
            _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState116
        | I8 ->
            _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState116
        | LBRACE ->
            _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState116
        | LBRACKET ->
            _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState116
        | UID _v ->
            _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState116 _v
        | VOID ->
            _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState116
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState116)
    | RPAREN ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, (tos : ((Ll.ty * Ll.operand) list))) = _menhir_stack in
        let _v : ((Ll.ty * Ll.operand) list) =     ( List.rev tos ) in
        let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
        (match _menhir_s with
        | MenhirState114 ->
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
        | MenhirState149 ->
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

and _menhir_reduce93 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
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
                _menhir_run80 _menhir_env (Obj.magic _menhir_stack) MenhirState108 _v
            | INT _v ->
                _menhir_run79 _menhir_env (Obj.magic _menhir_stack) MenhirState108 _v
            | NULL ->
                _menhir_run78 _menhir_env (Obj.magic _menhir_stack) MenhirState108
            | UID _v ->
                _menhir_run77 _menhir_env (Obj.magic _menhir_stack) MenhirState108 _v
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState108)
        | I64 ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | GID _v ->
                _menhir_run80 _menhir_env (Obj.magic _menhir_stack) MenhirState106 _v
            | INT _v ->
                _menhir_run79 _menhir_env (Obj.magic _menhir_stack) MenhirState106 _v
            | NULL ->
                _menhir_run78 _menhir_env (Obj.magic _menhir_stack) MenhirState106
            | UID _v ->
                _menhir_run77 _menhir_env (Obj.magic _menhir_stack) MenhirState106 _v
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState106)
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
    | MenhirState175 | MenhirState64 | MenhirState145 ->
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
                    _menhir_run80 _menhir_env (Obj.magic _menhir_stack) MenhirState160 _v
                | INT _v ->
                    _menhir_run79 _menhir_env (Obj.magic _menhir_stack) MenhirState160 _v
                | NULL ->
                    _menhir_run78 _menhir_env (Obj.magic _menhir_stack) MenhirState160
                | UID _v ->
                    _menhir_run77 _menhir_env (Obj.magic _menhir_stack) MenhirState160 _v
                | _ ->
                    assert (not _menhir_env._menhir_error);
                    _menhir_env._menhir_error <- true;
                    _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState160)
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
                _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState154
            | I64 ->
                _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState154
            | I8 ->
                _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState154
            | LBRACE ->
                _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState154
            | LBRACKET ->
                _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState154
            | UID _v ->
                _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState154 _v
            | VOID ->
                _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState154
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState154)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState169 ->
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
        _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState89
    | I64 ->
        _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState89
    | I8 ->
        _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState89
    | LBRACE ->
        _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState89
    | LBRACKET ->
        _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState89
    | UID _v ->
        _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState89 _v
    | VOID ->
        _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState89
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState89

and _menhir_goto_bop : _menhir_env -> 'ttv_tail -> (Ll.bop) -> 'ttv_return =
  fun _menhir_env _menhir_stack _v ->
    let _menhir_stack = (_menhir_stack, _v) in
    let _menhir_stack = Obj.magic _menhir_stack in
    assert (not _menhir_env._menhir_error);
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | I1 ->
        _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState133
    | I64 ->
        _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState133
    | I8 ->
        _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState133
    | LBRACE ->
        _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState133
    | LBRACKET ->
        _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState133
    | UID _v ->
        _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState133 _v
    | VOID ->
        _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState133
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState133

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
                _menhir_run146 _menhir_env (Obj.magic _menhir_stack) MenhirState175
            | STORE ->
                _menhir_run138 _menhir_env (Obj.magic _menhir_stack) MenhirState175
            | UID _v ->
                _menhir_run65 _menhir_env (Obj.magic _menhir_stack) MenhirState175 _v
            | BR | RET ->
                _menhir_reduce63 _menhir_env (Obj.magic _menhir_stack) MenhirState175
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState175)
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
    | MenhirState76 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let (((((_menhir_stack, _menhir_s, (u : (string))), _, (_4 : (Ll.ty))), _), _, (t : (Ll.ty))), _, (o : (Ll.operand))) = _menhir_stack in
        let _5 = () in
        let _3 = () in
        let _2 = () in
        let _v : (Ll.uid * Ll.insn) =     ( (u, Load (t,o)) ) in
        _menhir_goto_insn _menhir_env _menhir_stack _menhir_s _v
    | MenhirState90 ->
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
                _menhir_run80 _menhir_env (Obj.magic _menhir_stack) MenhirState92 _v
            | INT _v ->
                _menhir_run79 _menhir_env (Obj.magic _menhir_stack) MenhirState92 _v
            | NULL ->
                _menhir_run78 _menhir_env (Obj.magic _menhir_stack) MenhirState92
            | UID _v ->
                _menhir_run77 _menhir_env (Obj.magic _menhir_stack) MenhirState92 _v
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState92)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState92 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let (((((_menhir_stack, _menhir_s, (u : (string))), (c : (Ll.cnd))), _, (t : (Ll.ty))), _, (o1 : (Ll.operand))), _, (o2 : (Ll.operand))) = _menhir_stack in
        let _7 = () in
        let _3 = () in
        let _2 = () in
        let _v : (Ll.uid * Ll.insn) =     ( (u, Icmp (c,t,o1,o2)) ) in
        _menhir_goto_insn _menhir_env _menhir_stack _menhir_s _v
    | MenhirState97 ->
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
                    _menhir_run80 _menhir_env (Obj.magic _menhir_stack) MenhirState102 _v
                | INT _v ->
                    _menhir_run79 _menhir_env (Obj.magic _menhir_stack) MenhirState102 _v
                | NULL ->
                    _menhir_run78 _menhir_env (Obj.magic _menhir_stack) MenhirState102
                | UID _v ->
                    _menhir_run77 _menhir_env (Obj.magic _menhir_stack) MenhirState102 _v
                | _ ->
                    assert (not _menhir_env._menhir_error);
                    _menhir_env._menhir_error <- true;
                    _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState102)
            | I64 ->
                let _menhir_stack = Obj.magic _menhir_stack in
                let _menhir_env = _menhir_discard _menhir_env in
                let _tok = _menhir_env._menhir_token in
                (match _tok with
                | GID _v ->
                    _menhir_run80 _menhir_env (Obj.magic _menhir_stack) MenhirState100 _v
                | INT _v ->
                    _menhir_run79 _menhir_env (Obj.magic _menhir_stack) MenhirState100 _v
                | NULL ->
                    _menhir_run78 _menhir_env (Obj.magic _menhir_stack) MenhirState100
                | UID _v ->
                    _menhir_run77 _menhir_env (Obj.magic _menhir_stack) MenhirState100 _v
                | _ ->
                    assert (not _menhir_env._menhir_error);
                    _menhir_env._menhir_error <- true;
                    _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState100)
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
    | MenhirState100 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _, (o : (Ll.operand))) = _menhir_stack in
        let _1 = () in
        let _v : (Ll.operand list) =       ( [o] ) in
        _menhir_goto_i_operand_list_rev _menhir_env _menhir_stack _v
    | MenhirState102 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _, (o : (Ll.operand))) = _menhir_stack in
        let _1 = () in
        let _v : (Ll.operand list) =     ( [o] ) in
        _menhir_goto_i_operand_list_rev _menhir_env _menhir_stack _v
    | MenhirState106 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let ((_menhir_stack, (os : (Ll.operand list))), _, (o : (Ll.operand))) = _menhir_stack in
        let _3 = () in
        let _2 = () in
        let _v : (Ll.operand list) =     ( o::os ) in
        _menhir_goto_i_operand_list_rev _menhir_env _menhir_stack _v
    | MenhirState108 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let ((_menhir_stack, (os : (Ll.operand list))), _, (o : (Ll.operand))) = _menhir_stack in
        let _3 = () in
        let _2 = () in
        let _v : (Ll.operand list) =     ( o::os ) in
        _menhir_goto_i_operand_list_rev _menhir_env _menhir_stack _v
    | MenhirState112 ->
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
                _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState114
            | I64 ->
                _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState114
            | I8 ->
                _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState114
            | LBRACE ->
                _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState114
            | LBRACKET ->
                _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState114
            | UID _v ->
                _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState114 _v
            | VOID ->
                _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState114
            | COMMA | RPAREN ->
                _menhir_reduce93 _menhir_env (Obj.magic _menhir_stack) MenhirState114
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState114)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState117 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let (((_menhir_stack, _menhir_s, (tos : ((Ll.ty * Ll.operand) list))), _, (t : (Ll.ty))), _, (o : (Ll.operand))) = _menhir_stack in
        let _2 = () in
        let _v : ((Ll.ty * Ll.operand) list) =     ( (t,o)::tos ) in
        _menhir_goto_ty_operand_list_rev _menhir_env _menhir_stack _menhir_s _v
    | MenhirState121 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let ((_menhir_stack, _menhir_s, (t : (Ll.ty))), _, (o : (Ll.operand))) = _menhir_stack in
        let _v : ((Ll.ty * Ll.operand) list) =     ( [(t,o)] ) in
        _menhir_goto_ty_operand_list_rev _menhir_env _menhir_stack _menhir_s _v
    | MenhirState124 ->
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
                _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState126
            | I64 ->
                _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState126
            | I8 ->
                _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState126
            | LBRACE ->
                _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState126
            | LBRACKET ->
                _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState126
            | UID _v ->
                _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState126 _v
            | VOID ->
                _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState126
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState126)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState134 ->
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
                _menhir_run80 _menhir_env (Obj.magic _menhir_stack) MenhirState136 _v
            | INT _v ->
                _menhir_run79 _menhir_env (Obj.magic _menhir_stack) MenhirState136 _v
            | NULL ->
                _menhir_run78 _menhir_env (Obj.magic _menhir_stack) MenhirState136
            | UID _v ->
                _menhir_run77 _menhir_env (Obj.magic _menhir_stack) MenhirState136 _v
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState136)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState136 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let (((((_menhir_stack, _menhir_s, (u : (string))), (b : (Ll.bop))), _, (t : (Ll.ty))), _, (o1 : (Ll.operand))), _, (o2 : (Ll.operand))) = _menhir_stack in
        let _6 = () in
        let _2 = () in
        let _v : (Ll.uid * Ll.insn) =     ( (u, Binop (b,t,o1,o2)) ) in
        _menhir_goto_insn _menhir_env _menhir_stack _menhir_s _v
    | MenhirState139 ->
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
                _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState141
            | I64 ->
                _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState141
            | I8 ->
                _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState141
            | LBRACE ->
                _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState141
            | LBRACKET ->
                _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState141
            | UID _v ->
                _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState141 _v
            | VOID ->
                _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState141
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState141)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState142 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let (((((_menhir_stack, _menhir_s), _, (t1 : (Ll.ty))), _, (o1 : (Ll.operand))), _, (t2 : (Ll.ty))), _, (o2 : (Ll.operand))) = _menhir_stack in
        let _4 = () in
        let _1 = () in
        let _v : (Ll.uid * Ll.insn) =     ( (gensym "store", Store (t1,o1,o2)) ) in
        _menhir_goto_insn _menhir_env _menhir_stack _menhir_s _v
    | MenhirState147 ->
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
                _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState149
            | I64 ->
                _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState149
            | I8 ->
                _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState149
            | LBRACE ->
                _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState149
            | LBRACKET ->
                _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState149
            | UID _v ->
                _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState149 _v
            | VOID ->
                _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState149
            | COMMA | RPAREN ->
                _menhir_reduce93 _menhir_env (Obj.magic _menhir_stack) MenhirState149
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState149)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState155 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let ((_menhir_stack, _, (t : (Ll.ty))), _, (o : (Ll.operand))) = _menhir_stack in
        let _1 = () in
        let _v : (Ll.terminator) =     ( Ret (t, Some o) ) in
        _menhir_goto_terminator _menhir_env _menhir_stack _v
    | MenhirState160 ->
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

and _menhir_reduce63 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _v : ((Ll.uid * Ll.insn) list) =     ( [] ) in
    _menhir_goto_list_insn_ _menhir_env _menhir_stack _menhir_s _v

and _menhir_run65 : _menhir_env -> 'ttv_tail -> _menhir_state -> (string) -> 'ttv_return =
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
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState130)
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
        | CALL ->
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
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState111)
        | GEP ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | I1 ->
                _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState94
            | I64 ->
                _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState94
            | I8 ->
                _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState94
            | LBRACE ->
                _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState94
            | LBRACKET ->
                _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState94
            | UID _v ->
                _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState94 _v
            | VOID ->
                _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState94
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState94)
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
                _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState73
            | I64 ->
                _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState73
            | I8 ->
                _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState73
            | LBRACE ->
                _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState73
            | LBRACKET ->
                _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState73
            | UID _v ->
                _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState73 _v
            | VOID ->
                _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState73
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState73)
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

and _menhir_run138 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
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
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState138

and _menhir_run146 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
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
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState146

and _menhir_reduce38 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
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

and _menhir_goto_separated_nonempty_list_COMMA_ty_ : _menhir_env -> 'ttv_tail -> _menhir_state -> (Ll.ty list) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    match _menhir_s with
    | MenhirState186 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let (xs : (Ll.ty list)) = _v in
        let ((_menhir_stack, _menhir_s, (x : (Ll.ty))), _) = _menhir_stack in
        let _2 = () in
        let _v : (Ll.ty list) =     ( x :: xs ) in
        _menhir_goto_separated_nonempty_list_COMMA_ty_ _menhir_env _menhir_stack _menhir_s _v
    | MenhirState184 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let (x : (Ll.ty list)) = _v in
        let _v : (Ll.ty list) =     ( x ) in
        _menhir_goto_loption_separated_nonempty_list_COMMA_ty__ _menhir_env _menhir_stack _menhir_s _v
    | _ ->
        _menhir_fail ()

and _menhir_goto_loption_separated_nonempty_list_COMMA_ty__ : _menhir_env -> 'ttv_tail -> _menhir_state -> (Ll.ty list) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    let _menhir_stack = Obj.magic _menhir_stack in
    assert (not _menhir_env._menhir_error);
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | RPAREN ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_env = _menhir_discard _menhir_env in
        let _menhir_stack = Obj.magic _menhir_stack in
        let (((_menhir_stack, _, (rt : (Ll.ty))), _, (g : (string))), _, (xs0 : (Ll.ty list))) = _menhir_stack in
        let _6 = () in
        let _4 = () in
        let _1 = () in
        let _v : (Ll.gid * Ll.ty) = let ts =
          let xs = xs0 in
              ( xs )
        in
            ( (g, Fun (ts,rt)) ) in
        _menhir_goto_edecl _menhir_env _menhir_stack _v
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s

and _menhir_goto_terminator : _menhir_env -> 'ttv_tail -> (Ll.terminator) -> 'ttv_return =
  fun _menhir_env _menhir_stack _v ->
    let _menhir_stack = Obj.magic _menhir_stack in
    let _menhir_stack = Obj.magic _menhir_stack in
    let (t : (Ll.terminator)) = _v in
    let (_menhir_stack, _menhir_s, (is : ((Ll.uid * Ll.insn) list))) = _menhir_stack in
    let _v : (Ll.block) =     ( { insns = is; terminator=t }  ) in
    match _menhir_s with
    | MenhirState145 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let (b : (Ll.block)) = _v in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        let _2 = () in
        let _1 = () in
        let _v : (Ll.block) =     ( b ) in
        _menhir_goto_entry_block _menhir_env _menhir_stack _menhir_s _v
    | MenhirState175 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let (b : (Ll.block)) = _v in
        let ((_menhir_stack, (bs : ((Ll.lbl * Ll.block) list))), (l : (string))) = _menhir_stack in
        let _3 = () in
        let _v : ((Ll.lbl * Ll.block) list) =     ( (l,b) :: bs ) in
        _menhir_goto_block_list_rev _menhir_env _menhir_stack _v
    | MenhirState64 ->
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
        _menhir_run146 _menhir_env (Obj.magic _menhir_stack) MenhirState169
    | STORE ->
        _menhir_run138 _menhir_env (Obj.magic _menhir_stack) MenhirState169
    | UID _v ->
        _menhir_run65 _menhir_env (Obj.magic _menhir_stack) MenhirState169 _v
    | BR | RET ->
        _menhir_reduce63 _menhir_env (Obj.magic _menhir_stack) MenhirState169
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState169

and _menhir_run77 : _menhir_env -> 'ttv_tail -> _menhir_state -> (string) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _menhir_stack = Obj.magic _menhir_stack in
    let (u : (string)) = _v in
    let _v : (Ll.operand) =     ( Id u ) in
    _menhir_goto_operand _menhir_env _menhir_stack _menhir_s _v

and _menhir_run78 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _menhir_stack = Obj.magic _menhir_stack in
    let _1 = () in
    let _v : (Ll.operand) =     ( Null ) in
    _menhir_goto_operand _menhir_env _menhir_stack _menhir_s _v

and _menhir_run79 : _menhir_env -> 'ttv_tail -> _menhir_state -> (int) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _menhir_stack = Obj.magic _menhir_stack in
    let (i : (int)) = _v in
    let _v : (Ll.operand) =     ( Const (Int64.of_int i) ) in
    _menhir_goto_operand _menhir_env _menhir_stack _menhir_s _v

and _menhir_run80 : _menhir_env -> 'ttv_tail -> _menhir_state -> (string) -> 'ttv_return =
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
            _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState60
        | I64 ->
            _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState60
        | I8 ->
            _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState60
        | LBRACE ->
            _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState60
        | LBRACKET ->
            _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState60
        | UID _v ->
            _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState60 _v
        | VOID ->
            _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState60
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState60)
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
                    _menhir_run146 _menhir_env (Obj.magic _menhir_stack) MenhirState64
                | ENTRY ->
                    let _menhir_stack = Obj.magic _menhir_stack in
                    let _menhir_s = MenhirState64 in
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
                            _menhir_run146 _menhir_env (Obj.magic _menhir_stack) MenhirState145
                        | STORE ->
                            _menhir_run138 _menhir_env (Obj.magic _menhir_stack) MenhirState145
                        | UID _v ->
                            _menhir_run65 _menhir_env (Obj.magic _menhir_stack) MenhirState145 _v
                        | BR | RET ->
                            _menhir_reduce63 _menhir_env (Obj.magic _menhir_stack) MenhirState145
                        | _ ->
                            assert (not _menhir_env._menhir_error);
                            _menhir_env._menhir_error <- true;
                            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState145)
                    | _ ->
                        assert (not _menhir_env._menhir_error);
                        _menhir_env._menhir_error <- true;
                        let _menhir_stack = Obj.magic _menhir_stack in
                        let (_menhir_stack, _menhir_s) = _menhir_stack in
                        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
                | STORE ->
                    _menhir_run138 _menhir_env (Obj.magic _menhir_stack) MenhirState64
                | UID _v ->
                    _menhir_run65 _menhir_env (Obj.magic _menhir_stack) MenhirState64 _v
                | BR | RET ->
                    _menhir_reduce63 _menhir_env (Obj.magic _menhir_stack) MenhirState64
                | _ ->
                    assert (not _menhir_env._menhir_error);
                    _menhir_env._menhir_error <- true;
                    _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState64)
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

and _menhir_goto_edecl : _menhir_env -> 'ttv_tail -> (Ll.gid * Ll.ty) -> 'ttv_return =
  fun _menhir_env _menhir_stack _v ->
    let _menhir_stack = Obj.magic _menhir_stack in
    let _menhir_stack = Obj.magic _menhir_stack in
    let (e : (Ll.gid * Ll.ty)) = _v in
    let (_menhir_stack, (ds : (Ll.prog))) = _menhir_stack in
    let _v : (Ll.prog) =     ( { ds with edecls = e :: ds.edecls }  ) in
    _menhir_goto_decls_rev _menhir_env _menhir_stack _v

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
        _menhir_reduce38 _menhir_env (Obj.magic _menhir_stack) MenhirState35
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
        _menhir_reduce38 _menhir_env (Obj.magic _menhir_stack) MenhirState37
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
        | DECLARE | DEFINE | EOF | GID _ | UID _ ->
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
    | MenhirState51 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | LPAREN ->
            _menhir_run19 _menhir_env (Obj.magic _menhir_stack) MenhirState52
        | STAR ->
            _menhir_run18 _menhir_env (Obj.magic _menhir_stack) MenhirState52
        | DECLARE | DEFINE | EOF | GID _ | UID _ ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, (g : (string))), _, (t : (Ll.ty))) = _menhir_stack in
            let _4 = () in
            let _3 = () in
            let _2 = () in
            let _v : (Ll.gid * Ll.ty) =     ( (g, t) ) in
            _menhir_goto_edecl _menhir_env _menhir_stack _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState52)
    | MenhirState53 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | GID _v ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_s = MenhirState54 in
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
                    _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState56
                | I64 ->
                    _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState56
                | I8 ->
                    _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState56
                | LBRACE ->
                    _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState56
                | LBRACKET ->
                    _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState56
                | UID _v ->
                    _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState56 _v
                | VOID ->
                    _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState56
                | COMMA | RPAREN ->
                    let _menhir_stack = Obj.magic _menhir_stack in
                    let _menhir_s = MenhirState56 in
                    let _v : ((Ll.ty * Ll.uid) list) =     ( [] ) in
                    _menhir_goto_arg_list_rev _menhir_env _menhir_stack _menhir_s _v
                | _ ->
                    assert (not _menhir_env._menhir_error);
                    _menhir_env._menhir_error <- true;
                    _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState56)
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                let _menhir_stack = Obj.magic _menhir_stack in
                let (_menhir_stack, _menhir_s, _) = _menhir_stack in
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
        | LPAREN ->
            _menhir_run19 _menhir_env (Obj.magic _menhir_stack) MenhirState54
        | STAR ->
            _menhir_run18 _menhir_env (Obj.magic _menhir_stack) MenhirState54
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState54)
    | MenhirState60 | MenhirState56 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | LPAREN ->
            _menhir_run19 _menhir_env (Obj.magic _menhir_stack) MenhirState57
        | STAR ->
            _menhir_run18 _menhir_env (Obj.magic _menhir_stack) MenhirState57
        | UID _v ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_s = MenhirState57 in
            let _menhir_env = _menhir_discard _menhir_env in
            let _menhir_stack = Obj.magic _menhir_stack in
            let (u : (string)) = _v in
            let (_menhir_stack, _menhir_s, (t : (Ll.ty))) = _menhir_stack in
            let _v : (Ll.ty * Ll.uid) =                ( (t,u) ) in
            (match _menhir_s with
            | MenhirState60 ->
                let _menhir_stack = Obj.magic _menhir_stack in
                let _menhir_stack = Obj.magic _menhir_stack in
                let (a : (Ll.ty * Ll.uid)) = _v in
                let (_menhir_stack, _menhir_s, (args : ((Ll.ty * Ll.uid) list))) = _menhir_stack in
                let _2 = () in
                let _v : ((Ll.ty * Ll.uid) list) =     ( a::args ) in
                _menhir_goto_arg_list_rev _menhir_env _menhir_stack _menhir_s _v
            | MenhirState56 ->
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
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState57)
    | MenhirState73 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | COMMA ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_s = MenhirState74 in
            let _menhir_stack = (_menhir_stack, _menhir_s) in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | I1 ->
                _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState75
            | I64 ->
                _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState75
            | I8 ->
                _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState75
            | LBRACE ->
                _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState75
            | LBRACKET ->
                _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState75
            | UID _v ->
                _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState75 _v
            | VOID ->
                _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState75
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState75)
        | LPAREN ->
            _menhir_run19 _menhir_env (Obj.magic _menhir_stack) MenhirState74
        | STAR ->
            _menhir_run18 _menhir_env (Obj.magic _menhir_stack) MenhirState74
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState74)
    | MenhirState75 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | GID _v ->
            _menhir_run80 _menhir_env (Obj.magic _menhir_stack) MenhirState76 _v
        | INT _v ->
            _menhir_run79 _menhir_env (Obj.magic _menhir_stack) MenhirState76 _v
        | LPAREN ->
            _menhir_run19 _menhir_env (Obj.magic _menhir_stack) MenhirState76
        | NULL ->
            _menhir_run78 _menhir_env (Obj.magic _menhir_stack) MenhirState76
        | STAR ->
            _menhir_run18 _menhir_env (Obj.magic _menhir_stack) MenhirState76
        | UID _v ->
            _menhir_run77 _menhir_env (Obj.magic _menhir_stack) MenhirState76 _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState76)
    | MenhirState89 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | GID _v ->
            _menhir_run80 _menhir_env (Obj.magic _menhir_stack) MenhirState90 _v
        | INT _v ->
            _menhir_run79 _menhir_env (Obj.magic _menhir_stack) MenhirState90 _v
        | LPAREN ->
            _menhir_run19 _menhir_env (Obj.magic _menhir_stack) MenhirState90
        | NULL ->
            _menhir_run78 _menhir_env (Obj.magic _menhir_stack) MenhirState90
        | STAR ->
            _menhir_run18 _menhir_env (Obj.magic _menhir_stack) MenhirState90
        | UID _v ->
            _menhir_run77 _menhir_env (Obj.magic _menhir_stack) MenhirState90 _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState90)
    | MenhirState94 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | COMMA ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_s = MenhirState95 in
            let _menhir_stack = (_menhir_stack, _menhir_s) in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | I1 ->
                _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState96
            | I64 ->
                _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState96
            | I8 ->
                _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState96
            | LBRACE ->
                _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState96
            | LBRACKET ->
                _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState96
            | UID _v ->
                _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState96 _v
            | VOID ->
                _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState96
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState96)
        | LPAREN ->
            _menhir_run19 _menhir_env (Obj.magic _menhir_stack) MenhirState95
        | STAR ->
            _menhir_run18 _menhir_env (Obj.magic _menhir_stack) MenhirState95
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState95)
    | MenhirState96 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | GID _v ->
            _menhir_run80 _menhir_env (Obj.magic _menhir_stack) MenhirState97 _v
        | INT _v ->
            _menhir_run79 _menhir_env (Obj.magic _menhir_stack) MenhirState97 _v
        | LPAREN ->
            _menhir_run19 _menhir_env (Obj.magic _menhir_stack) MenhirState97
        | NULL ->
            _menhir_run78 _menhir_env (Obj.magic _menhir_stack) MenhirState97
        | STAR ->
            _menhir_run18 _menhir_env (Obj.magic _menhir_stack) MenhirState97
        | UID _v ->
            _menhir_run77 _menhir_env (Obj.magic _menhir_stack) MenhirState97 _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState97)
    | MenhirState111 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | GID _v ->
            _menhir_run80 _menhir_env (Obj.magic _menhir_stack) MenhirState112 _v
        | INT _v ->
            _menhir_run79 _menhir_env (Obj.magic _menhir_stack) MenhirState112 _v
        | LPAREN ->
            _menhir_run19 _menhir_env (Obj.magic _menhir_stack) MenhirState112
        | NULL ->
            _menhir_run78 _menhir_env (Obj.magic _menhir_stack) MenhirState112
        | STAR ->
            _menhir_run18 _menhir_env (Obj.magic _menhir_stack) MenhirState112
        | UID _v ->
            _menhir_run77 _menhir_env (Obj.magic _menhir_stack) MenhirState112 _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState112)
    | MenhirState116 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | GID _v ->
            _menhir_run80 _menhir_env (Obj.magic _menhir_stack) MenhirState117 _v
        | INT _v ->
            _menhir_run79 _menhir_env (Obj.magic _menhir_stack) MenhirState117 _v
        | LPAREN ->
            _menhir_run19 _menhir_env (Obj.magic _menhir_stack) MenhirState117
        | NULL ->
            _menhir_run78 _menhir_env (Obj.magic _menhir_stack) MenhirState117
        | STAR ->
            _menhir_run18 _menhir_env (Obj.magic _menhir_stack) MenhirState117
        | UID _v ->
            _menhir_run77 _menhir_env (Obj.magic _menhir_stack) MenhirState117 _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState117)
    | MenhirState149 | MenhirState114 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | GID _v ->
            _menhir_run80 _menhir_env (Obj.magic _menhir_stack) MenhirState121 _v
        | INT _v ->
            _menhir_run79 _menhir_env (Obj.magic _menhir_stack) MenhirState121 _v
        | LPAREN ->
            _menhir_run19 _menhir_env (Obj.magic _menhir_stack) MenhirState121
        | NULL ->
            _menhir_run78 _menhir_env (Obj.magic _menhir_stack) MenhirState121
        | STAR ->
            _menhir_run18 _menhir_env (Obj.magic _menhir_stack) MenhirState121
        | UID _v ->
            _menhir_run77 _menhir_env (Obj.magic _menhir_stack) MenhirState121 _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState121)
    | MenhirState123 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | GID _v ->
            _menhir_run80 _menhir_env (Obj.magic _menhir_stack) MenhirState124 _v
        | INT _v ->
            _menhir_run79 _menhir_env (Obj.magic _menhir_stack) MenhirState124 _v
        | LPAREN ->
            _menhir_run19 _menhir_env (Obj.magic _menhir_stack) MenhirState124
        | NULL ->
            _menhir_run78 _menhir_env (Obj.magic _menhir_stack) MenhirState124
        | STAR ->
            _menhir_run18 _menhir_env (Obj.magic _menhir_stack) MenhirState124
        | UID _v ->
            _menhir_run77 _menhir_env (Obj.magic _menhir_stack) MenhirState124 _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState124)
    | MenhirState126 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | LPAREN ->
            _menhir_run19 _menhir_env (Obj.magic _menhir_stack) MenhirState127
        | STAR ->
            _menhir_run18 _menhir_env (Obj.magic _menhir_stack) MenhirState127
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
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState127)
    | MenhirState130 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | LPAREN ->
            _menhir_run19 _menhir_env (Obj.magic _menhir_stack) MenhirState131
        | STAR ->
            _menhir_run18 _menhir_env (Obj.magic _menhir_stack) MenhirState131
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
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState131)
    | MenhirState133 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | GID _v ->
            _menhir_run80 _menhir_env (Obj.magic _menhir_stack) MenhirState134 _v
        | INT _v ->
            _menhir_run79 _menhir_env (Obj.magic _menhir_stack) MenhirState134 _v
        | LPAREN ->
            _menhir_run19 _menhir_env (Obj.magic _menhir_stack) MenhirState134
        | NULL ->
            _menhir_run78 _menhir_env (Obj.magic _menhir_stack) MenhirState134
        | STAR ->
            _menhir_run18 _menhir_env (Obj.magic _menhir_stack) MenhirState134
        | UID _v ->
            _menhir_run77 _menhir_env (Obj.magic _menhir_stack) MenhirState134 _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState134)
    | MenhirState138 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | GID _v ->
            _menhir_run80 _menhir_env (Obj.magic _menhir_stack) MenhirState139 _v
        | INT _v ->
            _menhir_run79 _menhir_env (Obj.magic _menhir_stack) MenhirState139 _v
        | LPAREN ->
            _menhir_run19 _menhir_env (Obj.magic _menhir_stack) MenhirState139
        | NULL ->
            _menhir_run78 _menhir_env (Obj.magic _menhir_stack) MenhirState139
        | STAR ->
            _menhir_run18 _menhir_env (Obj.magic _menhir_stack) MenhirState139
        | UID _v ->
            _menhir_run77 _menhir_env (Obj.magic _menhir_stack) MenhirState139 _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState139)
    | MenhirState141 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | GID _v ->
            _menhir_run80 _menhir_env (Obj.magic _menhir_stack) MenhirState142 _v
        | INT _v ->
            _menhir_run79 _menhir_env (Obj.magic _menhir_stack) MenhirState142 _v
        | LPAREN ->
            _menhir_run19 _menhir_env (Obj.magic _menhir_stack) MenhirState142
        | NULL ->
            _menhir_run78 _menhir_env (Obj.magic _menhir_stack) MenhirState142
        | STAR ->
            _menhir_run18 _menhir_env (Obj.magic _menhir_stack) MenhirState142
        | UID _v ->
            _menhir_run77 _menhir_env (Obj.magic _menhir_stack) MenhirState142 _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState142)
    | MenhirState146 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | GID _v ->
            _menhir_run80 _menhir_env (Obj.magic _menhir_stack) MenhirState147 _v
        | INT _v ->
            _menhir_run79 _menhir_env (Obj.magic _menhir_stack) MenhirState147 _v
        | LPAREN ->
            _menhir_run19 _menhir_env (Obj.magic _menhir_stack) MenhirState147
        | NULL ->
            _menhir_run78 _menhir_env (Obj.magic _menhir_stack) MenhirState147
        | STAR ->
            _menhir_run18 _menhir_env (Obj.magic _menhir_stack) MenhirState147
        | UID _v ->
            _menhir_run77 _menhir_env (Obj.magic _menhir_stack) MenhirState147 _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState147)
    | MenhirState154 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | GID _v ->
            _menhir_run80 _menhir_env (Obj.magic _menhir_stack) MenhirState155 _v
        | INT _v ->
            _menhir_run79 _menhir_env (Obj.magic _menhir_stack) MenhirState155 _v
        | LPAREN ->
            _menhir_run19 _menhir_env (Obj.magic _menhir_stack) MenhirState155
        | NULL ->
            _menhir_run78 _menhir_env (Obj.magic _menhir_stack) MenhirState155
        | STAR ->
            _menhir_run18 _menhir_env (Obj.magic _menhir_stack) MenhirState155
        | UID _v ->
            _menhir_run77 _menhir_env (Obj.magic _menhir_stack) MenhirState155 _v
        | LBL _ | RBRACE ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _, (t : (Ll.ty))) = _menhir_stack in
            let _1 = () in
            let _v : (Ll.terminator) =     ( Ret (t, None) ) in
            _menhir_goto_terminator _menhir_env _menhir_stack _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState155)
    | MenhirState181 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | GID _v ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_s = MenhirState182 in
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
                    _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState184
                | I64 ->
                    _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState184
                | I8 ->
                    _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState184
                | LBRACE ->
                    _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState184
                | LBRACKET ->
                    _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState184
                | UID _v ->
                    _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState184 _v
                | VOID ->
                    _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState184
                | RPAREN ->
                    let _menhir_stack = Obj.magic _menhir_stack in
                    let _menhir_s = MenhirState184 in
                    let _v : (Ll.ty list) =     ( [] ) in
                    _menhir_goto_loption_separated_nonempty_list_COMMA_ty__ _menhir_env _menhir_stack _menhir_s _v
                | _ ->
                    assert (not _menhir_env._menhir_error);
                    _menhir_env._menhir_error <- true;
                    _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState184)
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                let _menhir_stack = Obj.magic _menhir_stack in
                let (_menhir_stack, _menhir_s, _) = _menhir_stack in
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
        | LPAREN ->
            _menhir_run19 _menhir_env (Obj.magic _menhir_stack) MenhirState182
        | STAR ->
            _menhir_run18 _menhir_env (Obj.magic _menhir_stack) MenhirState182
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState182)
    | MenhirState186 | MenhirState184 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | COMMA ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_s = MenhirState185 in
            let _menhir_stack = (_menhir_stack, _menhir_s) in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | I1 ->
                _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState186
            | I64 ->
                _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState186
            | I8 ->
                _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState186
            | LBRACE ->
                _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState186
            | LBRACKET ->
                _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState186
            | UID _v ->
                _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState186 _v
            | VOID ->
                _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState186
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState186)
        | LPAREN ->
            _menhir_run19 _menhir_env (Obj.magic _menhir_stack) MenhirState185
        | STAR ->
            _menhir_run18 _menhir_env (Obj.magic _menhir_stack) MenhirState185
        | RPAREN ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, (x : (Ll.ty))) = _menhir_stack in
            let _v : (Ll.ty list) =     ( [ x ] ) in
            _menhir_goto_separated_nonempty_list_COMMA_ty_ _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState185)
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
    | MenhirState186 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState185 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState184 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState182 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState181 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        raise _eRR
    | MenhirState175 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        raise _eRR
    | MenhirState169 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState160 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        raise _eRR
    | MenhirState155 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState154 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        raise _eRR
    | MenhirState149 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState147 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState146 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState145 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState142 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState141 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState139 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState138 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState136 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState134 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState133 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let ((_menhir_stack, _menhir_s, _), _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState131 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState130 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState127 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState126 ->
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
    | MenhirState117 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState116 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState114 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState112 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState111 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState108 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        raise _eRR
    | MenhirState106 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        raise _eRR
    | MenhirState102 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        raise _eRR
    | MenhirState100 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        raise _eRR
    | MenhirState97 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState96 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState95 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState94 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState92 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState90 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState89 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let ((_menhir_stack, _menhir_s, _), _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState76 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState75 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState74 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState73 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState64 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState60 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState57 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState56 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState54 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState53 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        raise _eRR
    | MenhirState52 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState51 ->
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
    | DECLARE ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_env = _menhir_discard _menhir_env in
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | I1 ->
            _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState181
        | I64 ->
            _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState181
        | I8 ->
            _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState181
        | LBRACE ->
            _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState181
        | LBRACKET ->
            _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState181
        | UID _v ->
            _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState181 _v
        | VOID ->
            _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState181
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState181)
    | DEFINE ->
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
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState53)
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
            | EXTERNAL ->
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
                        _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState51
                    | I64 ->
                        _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState51
                    | I8 ->
                        _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState51
                    | LBRACE ->
                        _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState51
                    | LBRACKET ->
                        _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState51
                    | UID _v ->
                        _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState51 _v
                    | VOID ->
                        _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState51
                    | _ ->
                        assert (not _menhir_env._menhir_error);
                        _menhir_env._menhir_error <- true;
                        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState51)
                | _ ->
                    assert (not _menhir_env._menhir_error);
                    _menhir_env._menhir_error <- true;
                    let _menhir_stack = Obj.magic _menhir_stack in
                    raise _eRR)
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
      ; edecls = List.rev ds.edecls
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
    let _v : (Ll.prog) =     ( { tdecls = [] ; gdecls = [] ; fdecls = [] ; edecls = [] } ) in
    _menhir_goto_decls_rev _menhir_env _menhir_stack _v)
  

