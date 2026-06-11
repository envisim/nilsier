#ifndef KEYVALUEMAP_HEADER
#define KEYVALUEMAP_HEADER

#include <stddef.h>
#include <vector>

// InternalKey <ExternalKey, value>

class KeyValueMap {
public:
  int *keys_ = nullptr;
  size_t size_ = 0;
  std::vector<size_t> values_;

  KeyValueMap(int*, size_t);
  int GetExternalKey(size_t) const;
  size_t GetInternalKey(int) const;
  size_t GetValue(size_t) const;
  size_t Size() const;
};

#endif

