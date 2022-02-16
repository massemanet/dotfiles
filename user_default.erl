%%% Created :  7 Apr 2008 by Mats Cronqvist <masse@kreditor.se>

%% @doc
%% @end

-module('user_default').
-author('Mats Cronqvist').

-export([ineti/0,
	 sig/1, sig/2, sig/3,
	 print_source/1,
	 ports/0,
	 export_all/1,
	 tab/0,
	 flat/1, dump/1,
	 e/2,
	 kill/1,
	 pi/1, pi/2,
	 os/1,
	 scatter/0, scatter/1, scatter/2, scatter/3, scatter/4,
	 callstack/1,
	 bt/1,   %% foo
	 pid/1,
	 lm/0]).

%% recompiles M with export_all without access to the source.
export_all(M) ->
  case code:which(M) of
    non_existing -> no_such_module;
    F ->
      {ok, {_, [{abstract_code, {_, AC}}]}} = beam_lib:chunks(F, [abstract_code]),
      {ok, _, B} = compile:forms(AC, [export_all]),
      code:soft_purge(M),
      code:load_binary(M, F, B)
  end.

lm() ->
  MD5File =
    fun(F) ->
	case F of
	  preloaded -> dont_load;
	  _ ->
	    case beam_lib:md5(F) of
	      {ok, {_, MD5}} -> MD5;
	      _ -> dont_load
	    end
	end
    end,

  MD5Loaded =
    fun(M) ->
	case M:module_info(compile) of
	  [] -> dont_load;
	  _  -> M:module_info(md5)
	end
    end,

  Loadp =
    fun(M, F) ->
	Loaded = MD5Loaded(M),
	File = MD5File(F),
	Loaded =/= dont_load andalso
	  File =/= dont_load andalso
	  Loaded =/= File
    end,

  Load =
    fun(M, "") ->
	{cannot_load, M};
       (M, F) ->
	code:purge(M),
	{module, M} = code:load_abs(filename:rootname(F, ".beam")),
	M
    end,

  [Load(M, F) || {M, F} <- code:all_loaded(), Loadp(M, F)].

tab() ->
  N=node(),
  io:setopts([{expand_fun, fun(B)->rpc:call(N, edlin_expand, expand, [B]) end}]).

print_source(Mod) ->
  {ok, {_, [{"Abst", AChunk}]}} = beam_lib:chunks(code:which(Mod), ["Abst"]),
  {_, Forms} = binary_to_term(AChunk),
  io:fwrite("~s~n", [erl_prettypr:format(erl_syntax:form_list(Forms))]).

dump(Term)->
  File = filename:join([os:getenv("HOME"), "erlang.dump"]),
  {ok, FD}=file:open(File, [write]),
  try wr(FD, "~p.~n", Term), File
  after file:close(FD)
  end.

scatter() ->
    scatter(fun(X, Y) -> 1/(1+math:exp(X-Y)) end).

scatter(F) ->
    scatter(F, 5).

scatter(F, CS) ->
    scatter(F, CS, {-1, 1, 12}).

scatter(F, CS, D) ->
    scatter(F, CS, D, D).

scatter(Fun, CellSize, {Xmin, Xmax, Xsteps}, {Ymin, Ymax, Ysteps}) ->
    %% the "data model"
    S = fun(Min, Max, Steps) -> lists:map(fun(I) -> {I+1, Min+(I-1)*(Max-Min)/(Steps-1)} end, lists:seq(1, Steps)) end,
    XS = fun() -> S(Xmin, Xmax, Xsteps) end,
    YS = fun() -> S(Ymin, Ymax, Ysteps) end,
    VALS = fun(IVs) -> [V || {_, V} <- IVs] end,
    %% the canvas; a N+1 by M+1 matrix
    IXS = fun(C) -> lists:seq(1, tuple_size(element(1, C))) end,
    IYS = fun(C) -> lists:seq(1, tuple_size(C)) end,
    PEEK = fun(X, Y, C) -> element(X, element(Y, C)) end,
    POKE = fun(X, Y, V, C) -> setelement(Y, C, setelement(X, element(Y, C), V)) end,
    CANVAS = fun() -> list_to_tuple(lists:map(fun(Y) -> list_to_tuple([Y|VALS(XS())])  end, [''|VALS(YS())])) end,
    %% fill the matrix by calling `Fun/2'
    CELL = fun({{IX, X}, {IY, Y}}, C) -> POKE(IX, IY, Fun(X, Y), C) end,
    ROW = fun(X, C) -> lists:foldl(CELL, C, lists:map(fun(Y) -> {X, Y} end, YS())) end,
    FILL = fun(C) -> lists:foldl(ROW, C, XS()) end,
    %% print. each cell is `CellSize` wide
    FS = fun(CS, V) -> if is_integer(V) -> [CS, "w"]; is_float(V) -> [CS, ".2f"]; is_atom(V) -> [CS, "s"] end end,
    FORMAT = fun(V) -> lists:flatten([" ~", FS(integer_to_list(CellSize), V)]) end,
    EMIT = fun(V) -> io_lib:format(FORMAT(V), [V]) end,
    LINE = fun(IY, C) -> [lists:map(fun(IX) -> EMIT(PEEK(IX, IY, C)) end, IXS(C)), 10] end,
    TABLE = fun(C) -> lists:map(fun(IY) -> LINE(IY, C) end, IYS(C)) end,
    PRINT = fun(C) -> io:fwrite("~s", [TABLE(C)]) end,
    %% lambda all the way down
    PRINT(FILL(CANVAS())).

flat(Term) -> flat("~p~n", [Term]).
flat(Form, List) -> wr("~s", io_lib:format(Form, List)).

e(K, M) when is_map(M) -> maps:get(K, M);
e(N, T) when is_list(T) -> lists:nth(N, T);
e(N, T) when is_tuple(T) -> element(N, T).

kill(P) -> exit(pid(P), kill).

pi(P) -> process_info(pid(P)).
pi(P, Item) -> process_info(pid(P), Item).

os(Cmd) ->
  lists:foreach(fun(X)->wr("~s~n", X)end, string:tokens(os:cmd(Cmd), "\n")).

sig(M) ->
    Es = M:module_info(exports),
    sigs(M, fun(_, _F, _A, _) -> lists:member({_F, _A}, Es) end).
sig(M, F) ->
    sigs(M, fun(_, _F, _, _) -> _F==F end).
sig(M, F, A) ->
    sigs(M, fun(_, _F, _A, _) -> _F == F andalso _A == A end).

sigs(M, P) ->
    lists:append([[sig_format(M, F, As) || As <- Ass] || {F, A, Ass} <- sig_all(M), P(M, F, A, Ass)]).

sig_all(M) ->
    ABST = fun() -> e(1, e(3, binary_to_term(e(2, e(1, e(2, e(2, beam_lib:chunks(code:which(M), ["Dbgi"])))))))) end,
    FUNC = fun(E, O) -> [{e(3, E), e(4, E), [e(3, C) || C <- e(5, E), e(1, C) == clause]}|O] end,
    F = fun(E, O) when element(1, E) == function -> FUNC(E, O); (_, O) -> O end,
    lists:foldr(F, [], ABST()).

sig_format(M, F, As) ->
    lists:flatten(io_lib:format("~w:~w(~s)", [M, F, sig_argl(As)])).

sig_argl(As) ->
    string:join([sig_arg(A) || A <- As], ", ").

sig_arg(A) ->
    case e(1, A) of
	nil -> "[]";
	match -> sig_arg(e(3, A))++" = "++sig_arg(e(4, A));
	cons -> "["++sig_arg(e(3, A))++" | "++sig_arg(e(4, A))++"]";
	tuple -> "{"++sig_argl(e(3, A))++"}";
	var -> atom_to_list(e(3, A));
	_ -> try io_lib:format("~w", [e(3, A)]) catch _:_ -> error({A}) end
    end.

wr(F, E) -> wr(user, F, E).
wr(FD, F, E) -> io:fwrite(FD, F, [E]).

callstack(P) ->
  [string:strip(e(2, string:tokens(L, "(+)"))) || L<- bt(P), $0 =:= hd(L)].
bt(P) ->
  string:tokens(binary_to_list(e(2, process_info(pid(P), backtrace))), "\n").

pid(Pid) when is_list(Pid) -> list_to_pid(Pid);
pid(Pid) when is_pid(Pid) -> Pid;
pid(Atom) when is_atom(Atom) -> whereis(Atom);
pid({0, I2, I3}) when is_integer(I2) -> c:pid(0, I2, I3);
pid(I2) when is_integer(I2) -> pid({0, I2, 0}).

ineti() ->
  lists:foreach(fun ineti/1, ports()).

ineti(P) ->
  {_Fam, Type} = proplists:get_value(type, P),
  [Status|_]  = proplists:get_value(status, P),
  {LIP, LPort} = proplists:get_value(local, P),
  Sent        = proplists:get_value(sent, P),
  Recvd       = proplists:get_value(received, P),
  {RIP, RPort} =
    case proplists:get_value(remote, P) of
      enotconn -> {"*", "*"};
      {Rip, Rp} -> {inet_parse:ntoa(Rip), integer_to_list(Rp)}
    end,
  io:fwrite("~15s:~-5w ~15s:~-5s ~7w ~9w ~w/~w~n",
	    [inet_parse:ntoa(LIP), LPort, RIP, RPort, Type, Status, Sent, Recvd]).

ports() ->
  [port_info(P)++PI ||
    {P, PI}<-[{P, erlang:port_info(P)}||P<-erlang:ports()],
    lists:sublist(proplists:get_value(name, PI), 4, 100)=="_inet"].

port_info(P) ->
  {ok, Type} = prim_inet:gettype(P),
  {ok, Status} = prim_inet:getstatus(P),
  {ok, [{_, Sent}]} = prim_inet:getstat(P, [send_oct]),
  {ok, [{_, Recvd}]} = prim_inet:getstat(P, [recv_oct]),
  {ok, Local} = prim_inet:sockname(P),
  Remote = case prim_inet:peername(P) of
	     {ok, R} -> R;
	     {error, R} -> R
	   end,
  [{type, Type}, {status, Status},
   {sent, Sent}, {received, Recvd},
   {local, Local}, {remote, Remote}].
