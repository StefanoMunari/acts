use Mix.Config

config :broker_to_as, receiver_host: "localhost"
config :broker_to_as, receiver_port: 5672
config :broker_to_as, sender_host: "brk_fe_rabbitmq"
config :broker_to_as, sender_port: 5672
config :broker_to_as, mw_broker_xch: "mw_to_broker"
config :broker_to_as, broker_as_xch: "broker_to_as"
