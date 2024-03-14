ClickHouse Fuzzer
=========

ClickHouse Fuzzer is a small program listing all functions of different ClickHouse versions and detect (FUZZ) possible arguments to call them.
You will be able to directly access results in the /res folder under the Json format, once the script is ready.

# TODO
* Create the script
* Use results to fuzz for vulnerabilities inside ClickHouse

# Running ClickHouse
## Using docker
Inside the `running_clickhouse/docker` folder, you will find a `docker-compose.yml` and config files.
Run `docker-compose up -d` from that folder.

With this solution, docker introduces some latencies that can become a bottleneck.

## Manual
Follow [ClickHouse install documentation](https://clickhouse.com/docs/en/install) to install clickhouse-server.
Note: ClickHouse will add an entry in systemd, you might want to deactivate it.
```sh
sudo systemctl disable clickhouse-server.service
```

In order to not interfere with already existing ClickHouse, all files related to the one we are using will be under `running_clickhouse/host/<ch_version_to_fuzz>`.
Every exposed port by clickhouse have been incremented by 10000. E.g. http port is 18123 instead of 8123.

To start our ClickHouse server, run:
```sh
/usr/bin/clickhouse-server --config=running_clickhouse/host/<ch_version_to_fuzz>/config/config.xml --pid-file=running_clickhouse/host/<ch_version_to_fuzz>/clickhouse-server.pid

# Example for clickhouse 24.2
# If a newer version exists, you are welcome to open an issue to remind me to update this README :)
/usr/bin/clickhouse-server --config=running_clickhouse/host/ch24_2/config/config.xml --pid-file=running_clickhouse/host/ch24_2/clickhouse-server.pid
```
