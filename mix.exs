defmodule Hello.Mixfile do
  use Mix.Project

  def project do
    [app: :hello,
     version: "3.2.0",
     elixir: "~> 1.0",
     compilers: [:erlang, :elixir, :app],
     erlc_options: [{:parse_transform, :lager_transform}],
     deps: deps(Mix.env)]
  end

  def application do
    [applications: [:lager, :exometer_core, :cowboy, :ex_uri, :ezmq, :hackney, :jsx],
     env: [{:dnssd, false},
           {:metrics, [{:enabled, [:client, :listener, :handler]}]},
           {:default_protocol, :hello_proto_jsonrpc},
           {:transports, []},
           {:server_timeout, 10000},
           {:client_timeout, 10000}],
     mod: {:hello, []}]
  end

  defp deps(_) do
    [{:lager,         "~> 3.2.1", override: true},
     {:goldrush,      github: "DeadZen/goldrush", tag: "0.1.6", override: true},
     {:cowboy,        "~> 1.0.2"},
     {:yang,          github: "travelping/yang", branch: "master"},
     {:hackney,       "~> 1.4.4"},
     {:ex_uri,        github: "heroku/ex_uri", branch: "master"},
     {:ezmq,          github: "RoadRunnr/ezmq", branch: "fix-socket-crash"},
     {:jsx,           "~> 2.6.2"},
     {:msgpack,       "~> 0.6.0", override: true},
     {:dnssd,         github: "benoitc/dnssd_erlang", branch: "master", optional: true},
     {:meck,          "~> 0.8.2", override: true},
     {:exometer_core, github: "Feuerlabs/exometer_core", ref: "5fdd9426713a3c26cae32f644a3120711b1cdb64", override: true},
     {:edown,         github: "uwiger/edown", branch: "master", override: true}]
  end
end
