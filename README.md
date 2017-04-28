# contract-migrations

This is a contract deployment tool meant to be used with other haskell projects that interact with `Bloc`.
The expectations are something like this:

1. you should have a directory somewhere in your project where you keep all of your solidity contracts.
2. you should have a `contracts.yaml` file somewhere in your project (recommended: top-level or in your contracts director)

The layout of the `contracts.yaml` file is something like this:
```yaml
- name: Owned
  file: Owned.sol
  args:
    name: Bob
    age: '23'
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
-- Base.sol

contract Base {
...
}
```

```solidity
-- Simple.sol

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
    owner: Bob
    age: '25'
  value: '10'
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
