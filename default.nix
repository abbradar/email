{ mkDerivation, attoparsec, base, base64-bytestring, bytestring
, containers, email-header, exceptions, stdenv, text
}:
mkDerivation {
  pname = "mime-mail-parse";
  version = "0.1.0.0";
  src = ./.;
  buildDepends = [
    attoparsec base base64-bytestring bytestring containers
    email-header exceptions text
  ];
  license = stdenv.lib.licenses.bsd3;
}
