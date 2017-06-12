# contract-migrations

- [Getting Started](#getting-started)
- [Cirrus Indexing](#cirrus-indexing)
- [Command Line Tool](#command-line-tool)


## Getting Started
This is a contract deployment tool meant to be used either as a library with other haskell projects that interact with `Bloc`, or as a standalone command line tool. The expectations are something like this:

1. you should have a directory somewhere in your project where you keep all of your solidity contracts.
2. you should have a `contracts.yaml` file somewhere in your project (recommended: top-level or in your contracts director)

The layout of the `contracts.yaml` file is something like this:
```yaml
- name: Owned
  file: Owned.sol
  args:
    name: "Bob"
    age: 23
  txParams:
    gasLimit: 1
    gasPrice: 2
    nonce:  3
  value: 10

- name: IdentityAccessManager
  file: IdentityAccessManager.sol
```

Note: the `args`, `txParams` and `value` are all optional according to the `Bloc` api. 

*Don't specify the full filepath in the `file` argument*-- this migration tool assumes that the contract
filenames are unique within your project, i.e. you cannot have two different contracts named `Owned.sol`. The
`file` argument is really just the `filename` withouth a path, the tool does the rest for you.

Assume you have two seperate contracts, `.../contracts/Abstract/Base.sol` and `.../contracts/Concrete/Simple.sol` in seprate files, where `Simple` depends on `Base`:

```solidity
// Base.sol

contract Base {
...
}
```

```solidity
// Simple.sol

import ../Abstract/Base.sol

contract Simple is Base {
...
}
```
Again, the filepath is not important for the moment even in the import statements. It's more just an annotation for you at the moment to keep it straight where the contracts are.

If your `contracts.yaml` file looks like

```yaml
- name: Simple
  file: Simple.sol
  args:
    owner: "Bob"
    age: 25
  value: 10
```

Then when you run `runMigration` specifying the path to `contracts.yaml` and `/contracts`, the following contract will be
uploaded to `Bloc` with the args specified in the yaml file.

```solidity

contract Base {
...
}
contract Simple is Base {
...
}
```

## Cirrus Indexing

If you have contracts that you want to index in cirrus, you'll need to indicate this in your `contracts.yaml` file. Let's say you have contracts `StorageBlob` and `StorageBlobDeployer` as in the following:

```solidity
// StorageBlobDeployer.sol

contract StorageBlob {

  string contents;

  function StorageBlob(string _contents) {
    contents = _contents;
  }
}

contract StorageBlobDeployer {

  address[] blobs;
  
  function deployStorageBlob(string _contents) {
    StorageBlob blob = new StorageBlob(_contents);
    blobs.push(blob);
  }

}
```

If you would like to index the `StorageBlobs` you will be deploying, your `contracts.yaml` file would look like:

```yaml
- name: StorageBlobDeployer
  file: StorageBlobDeployer.sol
  index:
    - StorageBlob
```

In general, the `index` field will contain a list of contracts that you would like to index. These contracts should all be dependencies of the _entry point contract_, i.e. the one listed in the `name` field, so that when the contracts are gathered and uploaded they appear in that uploaded source code. 

## Command Line Tool
The command line tool is useful if you want to upload your contracts outside of an application, whether it's for expirementing or to view the generated ABIs. Assume as above that you have a `contracts` directory in your project where all of your contracts live, and a `contracts.yaml` file in your project root as above. If you inspect the `Makefile` you will see all of the environment variables which need to be filled out in order to use the tool, they are pretty self explanatory. You can then run `make deploy-contracts`, which will upload and index the contracts according to you `contracts.yaml` file, and write all of the build artifacts to a new directory based in `BUILD_ROOT`. **Warning** This will overwrite any existing directory named `build` in `BUILD_ROOT`.

The 0.0.1 release comes with a pre-templated contract. The project layout is as follows:

```
/contracts-uploader
  - Makefile
  - contract-uploader
  - contracts.yaml
  - /contracts
    - SimpleCounter.sol
```

with the following `contracts.yaml` file

```yaml
- file: SimpleCounter.sol
  name: SimpleCounter
  args:
    _initialAmount: 0
  index:
    - SimpleCounter

```

When you run `make deploy-contracts`, you should see logs indicating the successful upload and writing of build artifacts. If you did not specify the `ADMIN_ADDRESS` in the Makefile, a new admin account will be created for you when the script launches, and the uploaded contracts will be owned by this new address. The result of the command will be a newly created `build` directory with the following structure.

```
/build
  - admin.json
  - /contracts
    - /SimpleCounter
      - SimpleCounter.json
```

The `admin.json` file contains the address of the newly created admin account if a new one was created. The username, password pair for this address is what was specified in the `Makefile`. The `contracts` directory contains subdirectories for every contract you uploaded, with the ABIs written as `json` in the appropriate directories.
