file
====

A simple implementation of cross platform file class.

Motivation
----------
1. Easy to use.
2. Direct system call error reporting using `std::system_error`.

Example
-------
```cpp
#include "zpp/file.h"
#include <string_view>

using namespace std::string_view_literals;

int main()
{
    auto file = open("/tmp/file.txt", zpp::filesystem::open_mode::write);
    file.write("Hello World"sv);
    file.close();

    auto data = open("/tmp/file.txt", zpp::filesystem::open_mode::read).read();

    auto stdout_file = zpp::filesystem::weak_file(1);
    stdout_file.write(data);
}
```

API
---
There are two main class aliases `zpp::filesystem::file` and `zpp::filesystem::weak_file`, both
are a specialization of the `zpp::filesystem::basic_file` class.

The `zpp::filesystem::file` class is an owning class, whereas the `zpp::filesystem::weak_file` is
non-owning and can be constructed using a specific file handle or file descriptor without closing them.

In order to open a file, use one of the following overloads:

This overload is a passthrough to the platform API function, either `open` for Linux
or `CreateFile` for Windows, with the same parameters and order.
```cpp
/**
 * Open a file by path forwarding parameters to underlying platform
 * specific API.
 */
template <typename Char, typename... Arguments>
file open(const Char * path, Arguments... arguments)
```

This overload is a cross platform and more robust API to open a file.

```cpp
/**
 * Open mode.
 * - open_mode::read - read only, existing files only.
 * - open_mode::write - write only, truncate file if exists, create if does
 * not exist.
 * - open_mode::append - write only, append to existing file, create if
 * does not exist.
 * - open_mode::read_write - read and write, existing files only.
 * - open_mode::read_write_create - read and write, truncate file if
 * exists, create if does not exist.
 * - open_mode::read_write_append- read and write, append to existing file,
 * create if does not exist.
 */
enum class open_mode
{
    read,
    write,
    append,
    read_write,
    read_write_create,
    read_write_append,
};

/**
 * Open a file by path using a simple cross platform interface.
 */
template <typename Char>
file open(const Char * path, open_mode mode)
```

Overloads for `open` with `std::string_view` paths are also available for convenience.

For Windows UTF8 support, use either C++20 `char8_t` or define `ZPP_FILE_OPEN_CHAR_IS_UTF8`
in which case `char` strings are assumed to have UTF8 encoding inside open.

In order to transfer ownership of a regular file descriptor/handle into
a `zpp::filesystem::file`, create a `zpp::filesystem::file_handle` from the 
descriptor/handle and pass it to `zpp::filesystem::file` during construction.

The class aliases `zpp::filesystem::file` and `zpp::filesystem::weak_file` contain
the following API, which reports errors using `std::system_error`.

Note: the read and write APIs accept a `zpp::byte_view` or `zpp::cbyte_view` which
are similar to span except they implicitly allow all byte types - `char, unsigned char, std::byte`.
```cpp
/**
 * Attempts to read exactly the amount of bytes requested.
 * If not possible, an end_of_file_exception is thrown.
 */
void read_exact(byte_view data) const;

/**
 * Attempts to write exactly the amount of bytes requested.
 * If not possible, an insufficient_space_exception is thrown.
 */
void write_exact(cbyte_view data) const;

/**
 * Reads all requested bytes, unless the end of file is reached where
 * the reading stops. Returns the data inside a vector of bytes.
 * If the requested bytes are not specified, or set to zero, the
 * function reads the entire file.
 */
std::vector<std::byte> read(std::size_t size = {}) const;

/**
 * Reads all requested bytes, unless the end of file is reached where
 * the reading stops. The amount of bytes read is returned.
 */
std::size_t read(byte_view data) const;

/**
 * Write all of the given data to the file. May return
 * less bytes only if there is an insufficient space.
 */
std::size_t write(cbyte_view data) const;

/**
 * Reads from the file into the specified data byte array.
 * Executes a single read operation which may return less bytes
 * than requested. The amount of bytes read is returned.
 * If zero is returned, the end of file is reached.
 */
std::size_t read_once(byte_view data) const;

/**
 * Writes the given byte array to the file.
 * Executes a single write operation which may write less bytes
 * than requested. The amount of bytes written is returned.
 * If zero is returned there is insufficient space.
 */
std::size_t write_once(cbyte_view data) const;

/**
 * Returns the file size.
 */
std::uint64_t size() const;

/**
 * Returns true if the file is empty, else false.
 */
bool empty() const;

/**
 * Seeks into the file the given number of bytes, according
 * to the seek mode. Negative number of bytes is backward direction.
 * Returns the previous file pointer.
 */
std::uint64_t seek(std::int64_t offset, seek_mode mode) const;

/**
 * Returns the current file pointer.
 */
std::uint64_t tell() const;

/**
 * Truncates the file to the given size.
 * If the file is extended, the extended contents are unspecified.
 * If this function fails, the file position is unspecified.
 */
void truncate(std::uint64_t size) const;

/**
 * Sync the file.
 */
void sync() const;

/**
 * Closes the file.
 */
void close();

/**
 * Returns the file handle.
 */
auto get() const;

/**
 * Releases ownership of the file handle.
 */
auto release();
```

