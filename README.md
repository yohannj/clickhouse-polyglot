ClickHouse Polyglot
=========

ClickHouse Polyglot aims at providing libraries to build queries with the possibility to serialize and deserialize (part of) a query in different programming language.
The query is built as an abstract syntax tree, allowing inspecting and updating each part of the query.

<details>
  <summary>Table of Contents</summary><br>

- [Project Roadmap](#project-roadmap)
- [Repository Structure](#repository-structure)
- [Getting Started](#getting-started)
  - [Running ClickHouse](#running-clickhouse)
  - [Fuzzing function signatures](#fuzzing-function-signatures)
</details>

## Project Roadmap
### V0.1
  - [ ] Gather information about all available functions
    - [X] Run ClickHouse locally with a configuration allowing calling all functions
    - [X] Fuzz methods without combinators
    - [ ] 🚧 Aggregate method signatures
    - [ ] Determine which flags must be activated to use a function (e.g. `allow_experimental_funnel_functions=1`)
    - [ ] Parse ClickHouse documentation to determine argument names
  - [ ] Test that all fuzzed signature are working
  - [ ] Specify the serialized structure of the AST of the query
  - [ ] Build a first dynamically typed DSL

### V0.2
  - [ ] Gather more information about all available functions
    - [ ] Support combinators
    - [ ] Determine if a parameter/argument needs to be a constant or can be a column
    - [ ] Parse ClickHouse documentation to provide a short comment on each function (to be included in each function's documentation on the different language)

### V0.3
  - [ ] Build a second dynamically typed DSL
  - [ ] Document how to add a new dynamically typed DSL

### V0.4
  - [ ] Build a first statically typed DSL
  - [ ] Document how to add a new statically typed DSL

### Misc
  - [ ] Fix minor stability issues
    - [ ] Randomly ClickHouse gets stuck, not using any CPU or RAM (happens 2-3 times over the fuzzing of the 1500+ functions)
  - [ ] Find how to get ClickHouse running locally on macOS with all introspection functions (brew installation is not using `clickhouse-common-static-dbg` package)

## Repository Structure

```sh
└── ./
    ├── LICENSE
    ├── README.md
    ├── build.sbt
    │
    │   # Scala subproject
    │   # Collection of object and classes which are central to ClickHouse,
    │   # or nice to have for developer experience.
    │
    │   # E.g.
    │   # - ClickHouse's entities like the different types, supported timezones, ...
    │   # - Client to contact ClickHouse and parse the response
    │   # - Convenient functions for handling concurrency
    │   # - ...
    ├── common
    ├── project
    │
    │   # Contains available ClickHouse functions and their signatures
    │   # e.g. `functions_v24.5.txt` contains functions available in ClickHouse v24.5.x.x
    ├── res
    │
    │   # Run a local ClickHouse with custom configuration
    │   # Allow the usage of all functions in ClickHouse (ML, NLP, Geo dictionary, etc.) 
    ├── running_clickhouse       
    │   ├── docker
    │   └── host
    │
    │   # Scala subproject
    │   # Determine working signatures of all functions available in ClickHouse
    └── signature_fuzzer
```

## Getting started
### Running ClickHouse
You can run your own ClickHouse or use one of the provided ready-to-use template.
Few configurations are required to use specific functions.

It is suggested to run a local ClickHouse on your host, rather than going through a docker image, as docker-proxy is a bottleneck (cf [Medium article](https://franckpachot.medium.com/high-cpu-usage-in-docker-proxy-with-chatty-database-application-disable-userland-proxy-415ffa064955) around performance impact of docker-proxy in different kind of network call: `host <-> container` vs `same-container` vs `container1 <-> container2`)

#### Using docker
Inside the `running_clickhouse/docker` folder, you will find a `docker-compose.yml` and config files.
Run `docker-compose up -d` from that folder.

With this solution, docker introduces some latencies that can become a bottleneck.

#### On your host
##### Linux based host
###### Installation
Follow [ClickHouse install documentation](https://clickhouse.com/docs/en/install) to install clickhouse-server.

  Note: ClickHouse will add an entry in systemd, you might want to deactivate it.
  ```sh
  sudo systemctl disable clickhouse-server.service
  ```

###### Starting ClickHouse
In order to not interfere with already existing ClickHouse, all files related to the one we are using will be under `running_clickhouse/host/<ch_version_to_fuzz>`.
Every exposed port by clickhouse have been incremented by 10000. E.g. http port is 18123 instead of 8123.

To start our ClickHouse server, run:
```sh
/usr/bin/clickhouse-server --config=running_clickhouse/host/<ch_version_to_fuzz>/config/config.xml --pid-file=running_clickhouse/host/<ch_version_to_fuzz>/clickhouse-server.pid

# Example for clickhouse 24.2
/usr/bin/clickhouse-server --config=running_clickhouse/host/ch24_2/config/config.xml --pid-file=running_clickhouse/host/ch24_2/clickhouse-server.pid
```

N.B. You can create a file `running_clickhouse/host/<ch_version_to_fuzz>/config/config.d/z_path_update.xml` to update all paths based on where you cloned this project.
This file will be catched by gitignore so no worries about commiting it by mistake. At the same time it avoids accidentally committing an update in `path_update.xml`

##### macOS host
###### Installation
```sh
brew install --cask clickhouse
```

###### Starting ClickHouse
In order to not interfere with already existing ClickHouse, all files related to the one we are using will be under `running_clickhouse/host/<ch_version_to_fuzz>`.
Every exposed port by clickhouse have been incremented by 10000. E.g. http port is 18123 instead of 8123.

To start our ClickHouse server, run:
```sh
clickhouse server --config-file=running_clickhouse/host/<ch_version_to_fuzz>/config/config.xml --pidfile=running_clickhouse/host/<ch_version_to_fuzz>/clickhouse-server.pid

# Example for clickhouse 24.2
clickhouse server --config-file=running_clickhouse/host/ch24_2/config/config.xml --pidfile=running_clickhouse/host/ch24_2/clickhouse-server.pid
```

N.B. You can create a file `running_clickhouse/host/<ch_version_to_fuzz>/config/config.d/z_path_update.xml` to update all paths based on where you cloned this project.
This file will be catched by gitignore so no worries about commiting it by mistake. At the same time it avoids accidentally committing an update in `path_update.xml`

### Fuzzing function signatures
#### Catboost
Few ClickHouse functions require catboost. (As of writing this, only one function: [catboostEvaluate](https://clickhouse.com/docs/en/sql-reference/functions/other-functions#catboostevaluate))
If you want the fuzzer to find signatures for this function, you will need to download a `libcatboostmodel-*.so` (Linux) or `libcatboostmodel-*.dylib` (macOS) from [catboost releases](https://github.com/catboost/catboost/releases).

After that, copy `signature_fuzzer/src/main/ressources/reference.conf` to create a `signature_fuzzer/src/main/ressources/application.conf`.
Values in that file will overwrite settings in `reference.conf`, so you can set the path to where you downloaded libcatboostmodel. 
In addition, `application.conf` is part of the gitignore so no worries about commiting it by mistake.

#### Updating fuzzing settings
Few settings can be tweaked in `signature_fuzzer/src/main/ressources/reference.conf`.
If you only want to change values for your own usage, similarly as for catboost, create an `application.conf` to overwrite the different settings.

Part of the settings are the host/port where to contact ClickHouse (expects that a default account with no password can query ClickHouse).
There are also some settings used to improve fuzzing performances.

#### Running the fuzzer
Using `sbt`, execute:
```sh
sbt signature_fuzzer/run
```

It will create a file `res/functions_<ch_version_to_fuzz>.txt.part`.
As of writing, running it takes about 60 hours on my laptop (10 cores handling ClickHouse + the fuzzer), with about 2-3 times where ClickHouse stays alive but stops responding, so manual intervention required.
