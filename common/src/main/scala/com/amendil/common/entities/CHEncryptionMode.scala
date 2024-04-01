package com.amendil.common.entities

enum CHEncryptionMode(val name: String) {
  case aes_128_cbc extends CHEncryptionMode("aes-128-cbc")
  case aes_128_ctr extends CHEncryptionMode("aes-128-ctr")
  case aes_128_ecb extends CHEncryptionMode("aes-128-ecb")
  case aes_128_gcm extends CHEncryptionMode("aes-128-gcm")
  case aes_128_ofb extends CHEncryptionMode("aes-128-ofb")
  case aes_192_cbc extends CHEncryptionMode("aes-192-cbc")
  case aes_192_ctr extends CHEncryptionMode("aes-192-ctr")
  case aes_192_ecb extends CHEncryptionMode("aes-192-ecb")
  case aes_192_gcm extends CHEncryptionMode("aes-192-gcm")
  case aes_192_ofb extends CHEncryptionMode("aes-192-ofb")
  case aes_256_cbc extends CHEncryptionMode("aes-256-cbc")
  case aes_256_ctr extends CHEncryptionMode("aes-256-ctr")
  case aes_256_ecb extends CHEncryptionMode("aes-256-ecb")
  case aes_256_gcm extends CHEncryptionMode("aes-256-gcm")
  case aes_256_ofb extends CHEncryptionMode("aes-256-ofb")
}
