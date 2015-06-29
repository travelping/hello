defmodule UniMux.Mixfile do
  use Mix.Project

  def project do
    [app: :hello,
     version: "3.0.0",
     compilers: [:erlang, :app],
     erlc_options: [{:parse_transform, :lager_transform}],
     deps: deps(Mix.env)]
  end

  def application do
    [applications: [:lager, :cowboy, :ex_uri, :ezmq, :dnssd, :hackney, :jsx],
     mod: {:hello, []}]
  end

  defp deps(_) do
    [{:lager,   "~> 2.1.1", override: true},
     {:cowboy,  "~> 1.0.2"},
     {:yang,    github: "travelping/yang", branch: "master"},
     {:hackney, "~> 1.1.0"},
     {:ex_uri,  github: "heroku/ex_uri", branch: "master"},
     {:ezmq,    github: "RoadRunnr/ezmq", branch: "fix-socket-crash"},
     {:jsx,     "~> 2.6.2"},
     {:msgpack, github: "msgpack/msgpack-erlang", branch: "master"},
     {:dnssd,   github: "benoitc/dnssd_erlang", branch: "master"},
     {:meck,    "~> 0.8.2"}]
  end
end
