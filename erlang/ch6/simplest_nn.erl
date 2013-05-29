-module(simplest_nn).
-compile(export_all).

create() ->
  Weights = [random:uniform()-0.5,random:uniform()-0.5,random:uniform()-0.5],
  N_PId = spawn(?MODULE, neuron,[Weights, undefined, undefined]),
  S_PId = spawn(?MODULE, sensor,[N_PId]),
  A_PId = spawn(?MODULE, actuator,[N_PId]),
  N_PId ! {init,S_PId,A_PId},
  register(cortex, spawn(?MODULE, cortex,[S_PId,N_PId,A_PId])).


neuron(Weights,S_PId,A_PId) ->
  receive
    {S_PId, forward, Input} ->
      io:format("****Thinking****~nInput:~p~nwith Weights:~p~n",[Input,Weights]),
      Dot_Product = dot(Input, Weights, 0),
      Output = [math:tanh(Dot_Product)],
      A_PId ! {self(), forward, Output},
      neuron(Weights,S_PId,A_PId);
    {init, New_SPId, New_APId} ->
      neuron(Weights, New_SPId, New_APId);
    terminate ->
      ok
  end.

dot([I|Input],[W|Weights],Acc) ->
  dot(Input,Weights,I*W+Acc);
dot([],[],Acc) ->
  Acc;
dot([],[Bias],Acc) ->
  Acc + Bias.

sensor(N_PId) ->
  receive
    sync ->
      Sensory_Signal = [random:uniform(),random:uniform()],
      io:format("****Sensing****:~nSignal from the environment~p~n",[Sensory_Signal]),
      N_PId ! {self(),forward,Sensory_Signal},
      sensor(N_PId);
    terminate ->
      ok
  end.

actuator(N_PId) ->
  receive
    {N_PId,forward,Control_Signal} ->
      pts(Control_Signal),
      actuator(N_PId);
    terminate ->
      ok
  end.

pts(Control_Signal) ->
  io:format("****Acting****:~nUsing:~p to act on environment.~n", [Control_Signal]).

cortex(Sensor_PId,Neuron_PId,Actuator_PId) ->
  receive
    sense_think_act ->
      Sensor_PId ! sync,
      cortex(Sensor_PId,Neuron_PId,Actuator_PId);
    terminate ->
      Sensor_PId ! terminate,
      Neuron_PId ! terminate,
      Actuator_PId ! terminate,
      ok
  end.
