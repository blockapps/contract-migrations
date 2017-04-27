import ./StorageBlob.sol
import ../Modifiers/Owned.sol

contract BasicUserStorage is Owned {

  mapping(bytes32 => address) public userStorage;
  address public user;

  event Stored(
      address ownerKey,
      bytes32 ref,
      bytes32[] tags,
      string blobData
  );

  function BasicUserStorage(address _user) {
    user = _user;
  }

  function writeDataToStorage(address author, bytes32[] _tags, string _contents) onlyOwner returns(bytes32 contentsHash) {
    StorageBlob newBlob = new StorageBlob(user, author, _tags, _contents);
    userStorage[newBlob.hash()] = newBlob;
    Stored(user, newBlob.hash(), _tags, _contents);
    newBlob.addReader(user);
    newBlob.addReader(author);
    newBlob.addReader(owner);
    newBlob.transfer(owner);
    return newBlob.hash();
  }

  function getStorageBlob(bytes32 _hash) public returns(address) {
    return userStorage[_hash];
  }

  function giveReadPermission(address _newReader, bytes32 _hash) returns(bool success) {
    if (msg.sender == user || msg.sender == owner) {
      StorageBlob blob = StorageBlob(userStorage[_hash]);
      blob.addReader(_newReader);
      return true;
    } else {
      return true;
    }
  }
}
