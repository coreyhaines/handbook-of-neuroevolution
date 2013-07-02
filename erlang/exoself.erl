-module(exoself).
-compile(export_all).
-include("records.hrl").

map() ->
  map(ffnn).

map(FileName) ->
  {ok, Genotype} = file:consult(FileName),
  spawn(exoself, map, [FileName, Genotype]).

map(FileName, Genotype) ->
  IdsNPIds = ets:new(idsNpids, [set, private]),
  [Cx|CerebralUnits] = Genotype,
  Sensor_Ids = Cx#cortex.sensor_ids,
  Actuator_Ids = Cx#cortex.actuator_ids,
  NIds = Cx#cortex.nids,
  spawn_CerebralUnits(IdsNPIds, cortex, [Cx#cortex.id]),
  spawn_CerebralUnits(IdsNPIds, sensor, Sensor_Ids),
  spawn_CerebralUnits(IdsNPIds, actuator, Actuator_Ids),
  spawn_CerebralUnits(IdsNPIds, neuron, NIds),
  link_CerebralUnits(CerebralUnits, IdsNPIds),
  link_Cortex(Cx, IdsNPIds),
  Cx_PId = ets:lookup_element(IdsNPIds, Cx#cortex.id, 2),
  receive
    {Cx_PId, backup, Neuron_IdsNWeights} ->
      U_Genotype = update_genotype(IdsNPIds, Genotype, Neuron_IdsNWeights),
      {ok, File} = file:open(FileName, write),
      lists:foreach(fun(X) -> io:format(File, "~p.~n", [X]) end, U_Genotype),
      file:close(File),
      io:format("Finished updating to file:~p~n", [FileName])
  end.

spawn_CerebralUnits(IdsNPIds, CerebralUnitType, [Id|Ids]) ->
  PId = CerebralUnitType:gen(self(), node()),
  ets:insert(IdsNPIds, {Id, PId}),
  ets:insert(IdsNPIds, {PId, id}),
  spawn_CerebralUnits(IdsNPIds, CerebralUnitType, Ids);

spawn_CerebralUnits(_IdsNPIds, _CerebralUnitType, []) ->
  true.

link_CerebralUnits([R|Records], IdsNPIds) when is_record(R, sensor) ->
  SId = R#sensor.id,
  SPId = ets:lookup_element(IdsNPIds, SId, 2),
  Cx_PId = ets:lookup_element(IdsNPIds, R#sensor.cx_id, 2),
  SName = R#sensor.name,
  Fanout_Ids = R#sensor.fanout_ids,
  Fanout_PIds = [ets:lookup_element(IdsNPIds, Id, 2) || Id <- Fanout_Ids],
  SPId ! {self(), {SId, Cx_PId, SName, R#sensor.vl, Fanout_PIds}},
  link_CerebralUnits(Records, IdsNPIds);
link_CerebralUnits([R|Records], IdsNPIds) when is_record(R, actuator) ->
  AId = R#actuator.id,
  APId = ets:lookup_element(IdsNPIds, AId, 2),
  Cx_PId = ets:lookup_element(IdsNPIds, R#actuator.cx_id, 2),
  AName = R#actuator.name,
  Fanin_Ids = R#actuator.fanin_ids,
  Fanin_PIds = [ets:lookup_element(IdsNPIds, Id, 2) || Id <- Fanin_Ids],
  APId ! {self(), {AId, Cx_PId, AName, Fanin_PIds}},
  link_CerebralUnits(Records, IdsNPIds);
link_CerebralUnits([R|Records], IdsNPIds) when is_record(R, neuron) ->
  NId = R#neuron.id,
  NPId = ets:lookup_element(IdsNPIds, NId, 2),
  Cx_PId = ets:lookup_element(IdsNPIds, R#neuron.cx_id, 2),
  AFName = R#neuron.af,
  Input_IdPs = R#neuron.input_idps,
  Output_Ids = R#neuron.output_ids,
  Input_PIdPs = convert_IdPs2PIdPs(IdsNPIds, Input_IdPs, []),
  Output_PIds = [ets:lookup_element(IdsNPIds, Id, 2) || Id <- Output_Ids],
  NPId ! {self(), {NId, Cx_PId, AFName, Input_PIdPs, Output_PIds}},
  link_CerebralUnits(Records, IdsNPIds);
link_CerebralUnits([], _IdsNPIds) ->
  ok.

convert_IdPs2PIdPs(_IdsNPIds, [{bias, Bias}], Acc) ->
  lists:reverse([Bias|Acc]);
convert_IdPs2PIdPs(IdsNPIds, [{Id, Weights}|Fanin_IdPs], Acc) ->
  convert_IdPs2PIdPs(IdsNPIds, Fanin_IdPs, [{ets:lookup_element(IdsNPIds, Id, 2), Weights} | Acc]).

link_Cortex(Cx, IdsNPIds) ->
  Cx_Id = Cx#cortex.id,
  Cx_PId = ets:lookup_element(IdsNPIds, Cx_Id, 2),
  SIds = Cx#cortex.sensor_ids,
  AIds = Cx#cortex.actuator_ids,
  NIds = Cx#cortex.nids,
  SPIds = [ets:lookup_element(IdsNPIds, SId, 2) || SId <- SIds],
  APIds = [ets:lookup_element(IdsNPIds, AId, 2) || AId <- AIds],
  NPIds = [ets:lookup_element(IdsNPIds, NId, 2) || NId <- NIds],
  Cx_PId ! {self(), {Cx_Id, SPIds, APIds, NPIds}, 1000}.

update_genotype(IdsNPIds, Genotype, [{N_Id, PIdPs}|WeightPs]) ->
  N = lists:keyfind(N_Id, 2, Genotype),
  io:format("PIdPs:~p~n", [PIdPs]),
  Updated_InputIdPs = convert_IdPs2PIdPs(IdsNPIds, PIdPs, []),
  U_N = N#neuron{input_idps = Updated_InputIdPs},
  U_Genotype = lists:keyreplace(N_Id, 2, Genotype, U_N),
  io:format("N:~p~n U_N:~p~n Genotype ~p~n U_Genotype:~p~n", [N, U_N, Genotype, U_Genotype]),
  update_genotype(IdsNPIds, U_Genotype, WeightPs);
update_genotype(_IdsNPIds, Genotype, []) ->
  Genotype.

convert_PIdPs2IdPs(IdsNPIds, [{PId, Weights}|Input_PIdPs], Acc) ->
  convert_PIdPs2IdPs(IdsNPIds, Input_PIdPs, [{ets:lookup_element(IdsNPIds,PId,2),Weights}|Acc]);
convert_PIdPs2IdPs(_IdsNPIds, [Bias], Acc) ->
  lists:reverse([{bias,Bias}|Acc]).



