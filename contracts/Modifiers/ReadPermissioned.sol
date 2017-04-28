import "./Owned.sol";

contract ReadPermissioned is Owned {

  mapping(address => bool) readers;
  modifier onlyReader() { if (isReader(msg.sender)) _ }
 
  event AddReader(address owner, address reader);

  function addReader(address reader) onlyOwner {
    AddReader(owner, reader);
    readers[reader] = true;    
  }

  function isReader(address reader) public returns(bool){
    return readers[reader];
  }
}
