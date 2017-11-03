use Mix.Config

config :forwarding, send_host: "mw_brk_rabbitmq"
config :forwarding, send_port: 5672
config :forwarding, mw_broker_xch: "mw_to_broker"
config :forwarding, sender: Forwarding.Proxy.RabbitSender
