ClickHouse Polyglot
=========

ClickHouse Polyglot aims at providing libraries to build queries with the possibility to serialize and deserialize (part of) a query in different programming language.<br>
The query is built as an abstract syntax tree, allowing inspecting and updating each part of the query.

<details>
  <summary>Table of Contents</summary><br>

- [Project objectives](#project-objectives)
  - [Allow services in different languages to build a query together](#allow-services-in-different-languages-to-build-a-query-together)
  - [Low overhead DSL](#low-overhead-dsl)
  - [Query introspection](#query-introspection)
  - [Security by design](#security-by-design)
- [Project Roadmap](#project-roadmap)
- [Repository Structure](#repository-structure)
- [Getting Started](#getting-started)
  - [Running ClickHouse](#running-clickhouse)
  - [Fuzzing function signatures](#fuzzing-function-signatures)
</details>

## Project objectives
There is still quite a lot to do with this project, so here is a quick peak at what we want to achieve

### Allow services in different languages to build a query together
#### Context
When you started working with ClickHouse, you had a web service in Python that was handling everything and had all your business logic to build queries.<br>
For scaling and cost reasons, you want to move to Go. However the rewriting all the business logic is going to take months.<br>

On the other hand, other teams are working with ClickHouse and one of them built a service in Java to optimize queries.

🏹 What clickhouse-polyglot offers is a Domain-Specific Language (DSL) that can be serialized/deserialized in different languages.

#### Example
<img width="764" alt="image" src="https://github.com/yohannj/clickhouse-polyglot/assets/10663802/bc152f97-0caa-44e2-8bb3-ee2ac6307432"><br>

In the example, the new Go service is deployed, but missing some features.

##### Step 1  
Someone calls our Go service with the parameter `userGroup = 1`. This part of the business logic hasn't been migrated yet.

##### Step 2
We contact the legacy service to retrieve the conditions `countryId=1 AND status = 'member'`.

With this we can build our query.<br>
The DSL in Go is typesafe, `countIf` expects a `Boolean`, but thanks so clickhouse-polyglot serialization format, we know the condition built in Python evaluates to a Boolean so that works just fine.<br>
We notice our caller have user rights permissions, he cannot access all the data in ClickHouse, and so we include the condition `countryId IN (1, 2, 3)`.

##### Step 3
The query is sent to the optimization service, which can deserialize it.<br>
Thanks to the Abstract Syntax Tree structure of the DSL, the service finds that the condition `countryId = 1` in `countIf` is actually already covered by the condition `countryId IN (1, 2, 3)` in the WHERE clause.

We can rewrite `countIf(countryId = 1 AND status = 'member')` into `countIf(status = 'member')`.

##### Step 4
We now send the optimized query to ClickHouse.  

##### Conclusion
We introduced some network overhead along the way, but are able to migrate smoothly from Python to Go, and avoided duplicating the code of an optimizer in multiple languages.

### Low overhead DSL
When building a query, whatever the language being used, clickhouse-polyglot tries to stay very close to how you are used to write a ClickHouse SQL query.<br>
Going from one language to another will not be about learning a new library from scratch!

All the following queries will be equivalent:
```sql
-- ClickHouse SQL
SELECT arrayMap(i -> i + 1, [number, number * 2]) as foo FROM numbers(10)
```

```python
# Python
from clickhouse_polyglot import *

numbers_table = chtables.system.numbers(10)
select(arrayMap(lambda i : i + 1, [numbers_table.number, numbers_table.number * 2]).as("foo").from(numbers_table)
```

```java
// Java
import clickhouse.polyglot.*;

CHNumbersTable numbersTable = CHTables.system.numbers(10)
select(arrayMap((i) -> i + 1, List.of(numbersTable.number, numbersTable.number * 2)).as("foo").from(numbersTable)
```

```scala
// Scala
import clickhouse.polyglot.*

val numbersTable = CHTables.system.numbers(10)
select(arrayMap((i) => i + 1, Seq(numbersTable.number, numbersTable.number * 2)).as("foo").from(numbersTable)
```

### Query introspection
One key concept of ClickHouse Polyglot is its Domain-Specific Language (DSL) which builds an Abstract Syntax Tree of a query, or part of a query.

In contrasts with a query built as a String, it means that you can inspect deeply a query to do some statistics, change part of it (ClickHouse does almost no query optimization!) or have type-safety when calling a function.

### Example
Let's take the query `SELECT arrayMap(i -> i + 1, [number, number * 2]) as foo FROM numbers(10)`<br>
It will use a data structure that looks like:

<img width="1053" alt="image" src="https://github.com/yohannj/clickhouse-polyglot/assets/10663802/5a774ada-5b87-437c-9c9d-b26c3e8b78a4">


### Security by design
Queries will not be written as raw Strings. ClickHouse polyglot will not support syntax like `query = "SELECT " + field + " FROM " + tableName`.

Instead all ClickHouse functions will be available in each libraries as well as anything that builds the SELECT query.<br>
When using a String, could be as an argument of a method, the name of a column in a table, or an alias of an expression (`count() AS 'alias'`), then that String will be escaped.

## Project Roadmap
### V0.1
  - [ ] Gather information about all available functions
    - [X] Run ClickHouse locally with a configuration allowing calling all functions
    - [X] Fuzz methods without combinators
    - [X] Aggregate method signatures
    - [X] Determine which flags must be activated to use a function (e.g. `allow_experimental_funnel_functions=1`)
    - [ ] 🚧 Parse ClickHouse documentation to determine argument names
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
You can run your own ClickHouse or use one of the provided ready-to-use template.<br>
Few configurations are required to use specific functions.

It is suggested to run a local ClickHouse on your host, rather than going through a docker image, as docker-proxy is a bottleneck (cf [Medium article](https://franckpachot.medium.com/high-cpu-usage-in-docker-proxy-with-chatty-database-application-disable-userland-proxy-415ffa064955) around performance impact of docker-proxy in different kind of network call: `host <-> container` vs `same-container` vs `container1 <-> container2`)

#### Using docker
Inside the `running_clickhouse/docker` folder, you will find a `docker-compose.yml` and config files.<br>
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
In order to not interfere with already existing ClickHouse, all files related to the one we are using will be under `running_clickhouse/host/<ch_version_to_fuzz>`.<br>
Every exposed port by clickhouse have been incremented by 10000. E.g. http port is 18123 instead of 8123.

To start our ClickHouse server, run:
```sh
/usr/bin/clickhouse-server --config=running_clickhouse/host/<ch_version_to_fuzz>/config/config.xml --pid-file=running_clickhouse/host/<ch_version_to_fuzz>/clickhouse-server.pid

# Example for clickhouse 24.2
/usr/bin/clickhouse-server --config=running_clickhouse/host/ch24_2/config/config.xml --pid-file=running_clickhouse/host/ch24_2/clickhouse-server.pid
```

N.B. You can create a file `running_clickhouse/host/<ch_version_to_fuzz>/config/config.d/z_path_update.xml` to update all paths based on where you cloned this project.<br>
This file will be catched by gitignore so no worries about commiting it by mistake. At the same time it avoids accidentally committing an update in `path_update.xml`

##### macOS host
###### Installation
```sh
brew install --cask clickhouse
```

###### Starting ClickHouse
In order to not interfere with already existing ClickHouse, all files related to the one we are using will be under `running_clickhouse/host/<ch_version_to_fuzz>`.<br>
Every exposed port by clickhouse have been incremented by 10000. E.g. http port is 18123 instead of 8123.

To start our ClickHouse server, run:
```sh
clickhouse server --config-file=running_clickhouse/host/<ch_version_to_fuzz>/config/config.xml --pidfile=running_clickhouse/host/<ch_version_to_fuzz>/clickhouse-server.pid

# Example for clickhouse 24.2
clickhouse server --config-file=running_clickhouse/host/ch24_2/config/config.xml --pidfile=running_clickhouse/host/ch24_2/clickhouse-server.pid
```

N.B. You can create a file `running_clickhouse/host/<ch_version_to_fuzz>/config/config.d/z_path_update.xml` to update all paths based on where you cloned this project.<br>
This file will be catched by gitignore so no worries about commiting it by mistake. At the same time it avoids accidentally committing an update in `path_update.xml`

### Fuzzing function signatures
#### Catboost
Few ClickHouse functions require catboost. (As of writing this, only one function: [catboostEvaluate](https://clickhouse.com/docs/en/sql-reference/functions/other-functions#catboostevaluate))<br>
If you want the fuzzer to find signatures for this function, you will need to download a `libcatboostmodel-*.so` (Linux) or `libcatboostmodel-*.dylib` (macOS) from [catboost releases](https://github.com/catboost/catboost/releases).

After that, copy `signature_fuzzer/src/main/ressources/reference.conf` to create a `signature_fuzzer/src/main/ressources/application.conf`.<br>
Values in that file will overwrite settings in `reference.conf`, so you can set the path to where you downloaded libcatboostmodel. <br>
In addition, `application.conf` is part of the gitignore so no worries about commiting it by mistake.

#### Updating fuzzing settings
Few settings can be tweaked in `common/src/main/ressources/reference.conf` and `signature_fuzzer/src/main/ressources/reference.conf`.<br>
If you only want to change values for your own usage, similarly as for catboost, create an `application.conf` to overwrite the different settings.

Part of the settings are the host/port where to contact ClickHouse (expects that a default account with no password can query ClickHouse).<br>
There are also some settings used to improve fuzzing performances.

#### Running the fuzzer
Using `sbt`, execute:
```sh
sbt signature_fuzzer/run
```

It will create two files `res/functions_<ch_version_to_fuzz>.txt.part` and `res/types_<ch_version_to_fuzz>.txt.part`.<br>
As of writing, running it takes about 60 hours on my laptop (10 cores handling ClickHouse + the fuzzer), with about 2-3 times where ClickHouse stays alive but stops responding, so manual intervention required.
