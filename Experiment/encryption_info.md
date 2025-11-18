# Encryption info

## How I generated the keys

```sh
openssl genrsa -out private.pem 2048
openssl rsa -pubout -in private.pem -out public.pem
```

## Encryption

Handled by JSCrypt:

```js
const crypt = new JSEncrypt();
const publicKeyString = `-----BEGIN PUBLIC KEY-----
...
-----END PUBLIC KEY-----
`;
crypt.setPublicKey(publicKeyString);
crypt.encrypt(...);
```

## How to decrypt

```sh
echo "CuzmJZbXkx0aiQP6dM05UkAavgVrGxAXlcKYz6ppc0q+8NLTgXSxfpQKqUHFJQs3Dd2ABdhTSa/cOphO3YYntJw1ogKEZUb1UJNkfKKAWUg0o1dMxkTYDr7TYVaOkURxBw95yJzvQnODXO/NUXijdK2TAo8BZypAbRMk0bSzjxCFxI5hCoZiWtPaSzhg4UNT9dAwcfz4ONZMBHX7fR2bzVxbafKn9CyAWtM2U8xWFB3GcPCoUWWWuPTCq+5l5ktqi5fELG8JQKKepRYTX2g/nLd5p2Sm+LVWNSG82QaKP80jWY/DavzvrwueCUk8py2L2BypxNPOU1sDQwFg2+aj5A==" | openssl base64 -d | openssl rsautl -decrypt -inkey private.pem
```
