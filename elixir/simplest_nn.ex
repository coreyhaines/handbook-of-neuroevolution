defmodule SimplestNN do
  def create do
    n_pid = ([:random.uniform() - 0.5, :random.uniform() - 0.5, :random.uniform() - 0.5]
      |> (fn weights -> Process.spawn(__MODULE__, :neuron, [weights, nil, nil]) end).())
    s_pid = Process.spawn(__MODULE__, :sensor, [n_pid])
    a_pid = Process.spawn(__MODULE__, :actuator, [n_pid])
    n_pid <- {:init, s_pid, a_pid}
    Process.spawn(__MODULE__, :cortex, [s_pid, n_pid, a_pid])
      |> Process.register(:cortex)
  end

  def neuron(weights, s_pid, a_pid) do
    receive do
      {s_pid, :forward, input} ->
        :io.format("****Thinking****~nInput:~p~nwith Weights:~p~n", [input, weights])
        output = dot(input, weights, 0) |> :math.tanh
        a_pid <- {self(), :forward, [output]}
        neuron(weights, s_pid, a_pid)
      {:init, new_spid, new_apid} ->
        neuron(weights, new_spid, new_apid)
      :terminate ->
        :ok
    end
  end

  def dot([i|input],[w|weights],acc) do
    dot(input, weights, i*w+acc)
  end
  def dot([], [], acc) do
    acc
  end
  def dot([], [bias], acc) do
    acc + bias
  end

  def sensor(n_pid) do
    receive do
      :sync ->
        sensory_signal = [:random.uniform(), :random.uniform()]
        :io.format("****Sensing****:~nSignal from the environment~p~n", [sensory_signal])
        n_pid <- {self(), :forward, sensory_signal}
        sensor(n_pid)
      :terminate ->
        :ok
    end
  end

  def actuator(n_pid) do
    receive do
      {n_pid, :forward, control_signal} ->
        :io.format("****Acting****:~nUsing:~p to act on environment.~n", [control_signal])
        actuator(n_pid)
      :terminate ->
        :ok
    end
  end

  def cortex(sensor_pid, neuron_pid, actuator_pid) do
    receive do
      :sense_think_act ->
        sensor_pid <- :sync
        cortex(sensor_pid, neuron_pid, actuator_pid)
      :terminate ->
        sensor_pid <- :terminate
        neuron_pid <- :terminate
        actuator_pid <- :terminate
        :ok
    end
  end
end
