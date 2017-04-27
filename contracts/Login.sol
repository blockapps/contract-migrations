import ./Modifiers/Owned.sol

contract Login is Owned {
  mapping (address => uint) public lastLogins;

  event LoggedIn(
    address user,
    address application,
    uint timestamp
  );

  function loginToApp(address application) onlyOwner returns(address) {
    uint timestamp = now;
    lastLogins[application] = timestamp;
    LoggedIn(msg.sender, application, timestamp);
    return application;
  }
}
