contract StorageBlob is ReadPermissioned {
  
  address public userOwner;
  address public author;
  bytes32 public hash;
  bytes32[] public tags;
  string private contents;

  event BlobWritten(
    address _userOwner,
    address _author,
    bytes32 _hash,
    bytes32[] _tags,
    string _contents
  );

  function StorageBlob(address _userOwner, address _author, bytes32[] _tags, string _contents) {
    userOwner = _userOwner;
    author = _author;
    hash = keccak256(_contents);
    tags = _tags;
    contents = _contents;   
    BlobWritten (_userOwner, _author, hash, _tags, _contents);
  }

  // note: this can only be called by an external function, not from another
  // contract as it returns a string.
  function getContents() onlyReader returns(string) {
    return contents;
  }
}
