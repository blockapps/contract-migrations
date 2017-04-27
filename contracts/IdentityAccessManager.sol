import ./Modifiers/Owned.sol
import ./Storage/BasicUserStorage.sol
import ./Login.sol

contract IdentityAccessManager is Owned {

  mapping (address => address) public userToStore;

  event IdentityCreated(
    address userKey,
    address newStore
  );
  
  event LoginCreated(
    address user,
    address loginAddress
  );


  function createIdentityAgent(address userKey) onlyOwner returns(address, address) {
    BasicUserStorage store = new BasicUserStorage(userKey);
    store.transfer(msg.sender);
    IdentityCreated(userKey, store);
    userToStore[userKey] = address(store);
    Login login = new Login();
    login.transfer(userKey);
    LoginCreated(userKey, login);
    return (address(store), address(login));
  }
}

