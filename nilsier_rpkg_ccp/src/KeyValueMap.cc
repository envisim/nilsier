#include <stddef.h>
#include <stdexcept>
#include <string>
#include <vector>

#include "KeyValueMap.h"

KeyValueMap::KeyValueMap(int *keys, size_t n) {
  if (n == 0) {
    throw std::range_error("(KeyValueMap::KeyValueMap) n = 0");
  }

  keys_ = keys;
  size_ = n;
  values_.reserve(n);

  return;
}

int KeyValueMap::GetExternalKey(size_t internal_key) const {
  if (internal_key >= size_) {
    throw std::out_of_range("(KeyValueMap::GetExternalKey) oob");
  }

  return keys_[internal_key];
}

size_t KeyValueMap::GetInternalKey(int external_key) const {
  size_t n = Size();

  for (size_t i = 0; i < n; i++) {
    if (keys_[i] == external_key) {
      return i;
    }
  }

  throw std::range_error("(KeyValueMap::GetInternalKey) key not found: " + std::to_string(external_key));
}

size_t KeyValueMap::GetValue(size_t internal_key) const {
  if (internal_key >= size_) {
    throw std::out_of_range("(KeyValueMap::GetValue) oob: " + std::to_string(internal_key));
  }

  return values_[internal_key];
}

size_t KeyValueMap::Size() const {
  return size_;
}
