#pragma once

#include <algorithm>
#include <cstddef>
#include <cstdint>
#include <iterator>
#include <limits>
#include <stdexcept>
#include <string_view>
#include <system_error>
#include <type_traits>
#include <utility>
#include <vector>

#if __has_include(<windows.h>)
#include <windows.h>
#define ZPP_FILE_WINDOWS
#else
#include <errno.h>
#include <fcntl.h>
#include <sys/stat.h>
#include <sys/types.h>
#include <unistd.h>
#endif

namespace zpp::filesystem
{
/**
 * File seek mode, end, set or current.
 * - seek_mode::set - relative to the beginning of the file.
 * - seek_mode::end - relative to the end of file.
 * - seek_mode::current - relative to the current position.
 */
enum class seek_mode
{
    set,
    end,
    current,
};

namespace detail
{
/**
 * Implements read, write, seek, close.
 */
template <typename File>
class basic_file_base
{
public:
    /**
     * Attempts to read exactly the amount of bytes requested.
     * If not possible, an end_of_file_exception is thrown.
     */
    void read_exact(void * data, std::size_t size) const;

    /**
     * Attempts to write exactly the amount of bytes requested.
     * If not possible, an insufficient_space_exception is thrown.
     */
    void write_exact(const void * data, std::size_t size) const;

    /**
     * Attempts to write exactly the amount of bytes requested.
     * If not possible, an insufficient_space_exception is thrown.
     * This overload is for string view.
     */
    template <typename Type>
    void write_exact(std::basic_string_view<Type> string) const;

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
    std::size_t read(void * data, std::size_t size) const;

    /**
     * Write all of the given data to the file. May return
     * less bytes only if there is an insufficient space.
     */
    std::size_t write(const void * data, std::size_t size) const;

    /**
     * Write all of the given data to the file. May return less
     * bytes only if there is an insufficient space.
     * This overload is for string view.
     */
    template <typename Type>
    std::size_t write(std::basic_string_view<Type> string) const;

    /**
     * Reads from the file into the specified data byte array.
     * Executes a single read operation which may return less bytes
     * than requested. The amount of bytes read is returned.
     * If zero is returned, the end of file is reached.
     */
    std::size_t read_once(void * data, std::size_t size) const;

    /**
     * Writes the given byte array to the file.
     * Executes a single write operation which may write less bytes
     * than requested. The amount of bytes written is returned.
     * If zero is returned there is insufficient space.
     */
    std::size_t write_once(const void * data, std::size_t size) const;

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

protected:
    /**
     * Protected destructor as this is not meant
     * to be destroyed externally.
     */
    ~basic_file_base() = default;

private:
    /**
     * Returns the derived file.
     */
    decltype(auto) derived()
    {
        return static_cast<File &>(*this);
    }

    /**
     * Returns the derived file.
     */
    decltype(auto) derived() const
    {
        return static_cast<const File &>(*this);
    }
};

} // namespace detail

/**
 * End of file is reached, the amount of bytes read is stored within.
 */
class end_of_file_exception : public std::runtime_error
{
public:
    /**
     * Creates an end of file exception.
     */
    explicit end_of_file_exception(std::size_t bytes_read) :
        std::runtime_error("End of file"),
        m_bytes_read(bytes_read)
    {
    }

    /**
     * Returns the amount of bytes read until the end of file.
     */
    std::size_t bytes_read() const
    {
        return m_bytes_read;
    }

private:
    std::size_t m_bytes_read;
};

/**
 * Insufficient space encountered while writing, the amount of bytes
 * written is stored within.
 */
class insufficient_space_exception : public std::runtime_error
{
public:
    /**
     * Creates an end of file exception.
     */
    explicit insufficient_space_exception(std::size_t bytes_written) :
        std::runtime_error("Insufficient space"),
        m_bytes_written(bytes_written)
    {
    }

    /**
     * Returns the amount of bytes written until the space limit reached.
     */
    std::size_t bytes_written() const
    {
        return m_bytes_written;
    }

private:
    std::size_t m_bytes_written;
};

/**
 * Represents a file.
 */
template <typename FileHandle, typename InvalidFileHandle>
class basic_file : public detail::basic_file_base<
                       basic_file<FileHandle, InvalidFileHandle>>
{
public:
    /**
     * Invalid file handle value.
     */
    inline static const auto invalid_file_handle =
        InvalidFileHandle::value;

    /**
     * The file handle type.
     */
    using file_handle = FileHandle;

    /**
     * Default constructs a file.
     */
    basic_file() = default;

    /**
     * Create a file from a handle.
     */
    basic_file(file_handle file_handle) : m_file(std::move(file_handle))
    {
    }

    /**
     * Swaps between this file and other.
     */
    void swap(basic_file & other) noexcept
    {
        using std::swap;
        swap(m_file, other.m_file);
    }

    /**
     * Returns the file handle.
     */
    auto get() const
    {
        if constexpr (std::is_class_v<file_handle>) {
            return m_file.get();
        } else {
            return m_file;
        }
    }

    /**
     * Releases ownership of the file handle.
     */
    auto release()
    {
        if constexpr (std::is_class_v<file_handle>) {
            return m_file.release();
        } else {
            auto file = m_file;
            m_file = invalid_file_handle;
            return file;
        }
    }

    /**
     * Returns true if valid, else false.
     */
    explicit operator bool() const
    {
        return (invalid_file_handle != m_file);
    }

private:
    /**
     * The file handle.
     */
    file_handle m_file{invalid_file_handle};
};

/**
 * Swaps between left file and right file.
 */
template <typename... Arguments>
void swap(basic_file<Arguments...> & left,
          basic_file<Arguments...> & right) noexcept
{
    left.swap(right);
}

//
// Platform specific code starts here.
//

/**
 * The file handle type.
 */
#ifdef ZPP_FILE_WINDOWS
struct invalid_file_handle
{
    inline static const auto value = INVALID_HANDLE_VALUE;
};
using weak_file_handle = HANDLE;
#else
struct invalid_file_handle
{
    static const auto value = -1;
};
using weak_file_handle = int;
#endif

/**
 * Represents a file handle owning class.
 */
class file_handle
{
public:
    /**
     * Default constructs a file handle.
     */
    file_handle() = default;

    /**
     * Create a file from a handle.
     */
    explicit file_handle(weak_file_handle handle) : m_file(handle)
    {
    }

    /**
     * Construct a file handle by move from other.
     */
    file_handle(file_handle && other) noexcept : m_file(other.release())
    {
    }

    /**
     * Assigns to this file handle by from other file handle.
     */
    file_handle & operator=(file_handle other) noexcept
    {
        other.swap(*this);
        return *this;
    }

    /**
     * Destroys the file handle, closing it if open.
     */
    ~file_handle()
    {
        if (invalid_file_handle::value != m_file) {
#ifdef ZPP_FILE_WINDOWS
            CloseHandle(m_file);
#else
            ::close(m_file);
#endif
        }
    }

    /**
     * Swaps between this file handle and other.
     */
    void swap(file_handle & other) noexcept
    {
        std::swap(m_file, other.m_file);
    }

    /**
     * Returns the weak file handle.
     */
    weak_file_handle get() const
    {
        return m_file;
    }

    /**
     * Releases ownership of the file handle.
     */
    weak_file_handle release()
    {
        auto handle = m_file;
        m_file = invalid_file_handle::value;
        return handle;
    }

private:
    /**
     * The weak file handle.
     */
    weak_file_handle m_file{invalid_file_handle::value};
};

/**
 * Swaps between left file handle and right file handle.
 */
inline void swap(file_handle & left, file_handle & right) noexcept
{
    left.swap(right);
}

/**
 * Alias file and weak_file
 * @{
 */
using file = basic_file<file_handle, invalid_file_handle>;
using weak_file = basic_file<weak_file_handle, invalid_file_handle>;
/**
 * @}
 */

} // namespace zpp::filesystem

//
// Implementation
//

namespace zpp::filesystem
{
namespace detail
{
template <typename File>
std::size_t basic_file_base<File>::read(void * data,
                                        std::size_t size) const
{
    auto byte_data = static_cast<std::byte *>(data);
    std::size_t bytes_read{};

    while (size > bytes_read) {
        // Perform a single read.
        auto result = read_once(byte_data + bytes_read, size - bytes_read);

        // If end of file, return.
        if (!result) {
            return bytes_read;
        }

        // Update bytes read.
        bytes_read += result;
    }

    return bytes_read;
}

template <typename File>
std::size_t basic_file_base<File>::write(const void * data,
                                         std::size_t size) const
{
    auto byte_data = static_cast<const std::byte *>(data);
    std::size_t bytes_written{};

    while (size > bytes_written) {
        // Perform a single write.
        auto result =
            write_once(byte_data + bytes_written, size - bytes_written);

        // If insufficient space, return.
        if (!result) {
            return bytes_written;
        }

        // Update bytes written.
        bytes_written += result;
    }

    return bytes_written;
}

template <typename File>
template <typename Type>
std::size_t
basic_file_base<File>::write(std::basic_string_view<Type> string) const
{
    return write(std::data(string), std::size(string));
}

template <typename File>
std::vector<std::byte> basic_file_base<File>::read(std::size_t size) const
{
    // In the absence of size and file size, this will be the initial size
    // of the vector.
    constexpr std::size_t initial_vector_size = 0x1000;

    // The file data.
    std::vector<std::byte> data;

    // If size is zero, we need to read the entire file,
    // try to reserve space according to the file size.
    if (!size) {
        // Get the file size.
        auto file_size = this->size();

        // If file size cannot be determined, resize to the initial size.
        if (!file_size) {
            // Resize to initial size.
            data.resize(initial_vector_size);
        } else {
            // Get the file offset.
            auto current_offset = tell();

            // If the offset is beyond the size, return immediately.
            if (current_offset >= file_size) {
                return data;
            }

            // Compute the amount of bytes to read.
            auto data_size = file_size - current_offset;

            // Check whether the data size is larger than the
            // implementation capacity.
            if (data_size > (std::numeric_limits<std::size_t>::max)()) {
                throw std::range_error(
                    "Amount of file data is too large for this platform.");
            }

            // Resize to the data size.
            data.resize(static_cast<std::size_t>(data_size));
        }
    } else {
        // Reserve the requested size.
        data.resize(size);
    }

    // The amount of bytes read.
    std::size_t bytes_read{};

    // Read the data.
    while (true) {
        // Compute the amount of bytes to read.
        std::size_t bytes_to_read = data.size() - bytes_read;

        // Perform the read operation.
        auto result = read(data.data() + bytes_read, bytes_to_read);

        // Update the bytes read.
        bytes_read += result;

        // If we read less than requested, the end of file is reached.
        if (result < bytes_to_read) {
            data.resize(bytes_read);
            break;
        }

        // If we did not finish reading, continue reading.
        if (data.size() < size) {
            // Resize the vector.
            data.resize(data.size() * 3 / 2);
            continue;
        }

        // Finished reading.
        break;
    }

    // Shrink the data if needed, and return.
    data.shrink_to_fit();
    return data;
}

template <typename File>
void basic_file_base<File>::read_exact(void * data, std::size_t size) const
{
    if (auto result = read(data, size); result != size) {
        throw end_of_file_exception(result);
    }
}

template <typename File>
void basic_file_base<File>::write_exact(const void * data,
                                        std::size_t size) const
{
    if (auto result = write(data, size); result != size) {
        throw insufficient_space_exception(result);
    }
}

template <typename File>
template <typename Type>
void basic_file_base<File>::write_exact(
    std::basic_string_view<Type> string) const
{
    if (auto result = write(string); result != string.size()) {
        throw insufficient_space_exception(result);
    }
}

template <typename File>
bool basic_file_base<File>::empty() const
{
    return !size();
}

#ifndef ZPP_FILE_WINDOWS
#ifndef ZPP_FILE_NO_64_BIT_SUPPORT
// Assert 64 bit support for off_t.
static_assert(sizeof(off_t) == sizeof(std::uint64_t) &&
                  sizeof(std::declval<struct stat>().st_size) ==
                      sizeof(std::uint64_t),
              "Linux file offsets are not 64 bit. To ignore, "
              "please define ZPP_FILE_NO_64_BIT_SUPPORT");
#endif
#endif

template <typename File>
std::size_t basic_file_base<File>::read_once(void * data,
                                             std::size_t size) const
{
#ifdef ZPP_FILE_WINDOWS
    // The maximum bytes windows can read at once.
    static constexpr std::size_t max_read_size =
        (std::numeric_limits<DWORD>::max)();

    // The amount of bytes to read at this time.
    auto bytes_to_read =
        static_cast<DWORD>((std::min)(size, max_read_size));

    // The bytes read.
    DWORD bytes_read{};

    // Read from the file.
    auto result = ReadFile(
        derived().get(), data, bytes_to_read, &bytes_read, nullptr);

    // If failed, throw an error.
    if (!result) {
        throw std::system_error(
            GetLastError(), std::system_category(), "ReadFile failed");
    }

    // Return the amount of bytes we read.
    return bytes_read;
#else
    int result = -1;

    // Perform a read in a non-interruptible manner.
    do {
        result = ::read(derived().get(), data, size);
    } while (-1 == result && EINTR == errno);

    // If failed, throw an error.
    if (-1 == result) {
        throw std::system_error(
            errno, std::system_category(), "read failed");
    }

    // Return the bytes read.
    return result;
#endif
}

template <typename File>
std::size_t basic_file_base<File>::write_once(const void * data,
                                              std::size_t size) const
{
#ifdef ZPP_FILE_WINDOWS
    // The maximum bytes windows can write at once.
    static constexpr std::size_t max_write_size =
        (std::numeric_limits<DWORD>::max)();

    // The amount of bytes to write at this time.
    auto bytes_to_write =
        static_cast<DWORD>((std::min)(size, max_write_size));

    // The bytes written.
    DWORD bytes_written{};

    // Write to the file.
    auto result = WriteFile(
        derived().get(), data, bytes_to_write, &bytes_written, nullptr);

    // If failed, throw an error.
    if (!result) {
        throw std::system_error(
            GetLastError(), std::system_category(), "WriteFile failed");
    }

    // Return the amount of bytes we wrote.
    return bytes_written;
#else
    int result = -1;

    // Perform a write in a non-interruptible manner.
    do {
        result = ::write(derived().get(), data, size);
    } while (-1 == result && EINTR == errno);

    // If failed, throw an error.
    if (-1 == result) {
        throw std::system_error(
            errno, std::system_category(), "write failed");
    }

    // Return the amount of bytes we wrote.
    return result;
#endif
}

template <typename File>
void basic_file_base<File>::close()
{
#ifdef ZPP_FILE_WINDOWS
    // Release ownership of the handle.
    auto handle = derived().release();

    // Close the handle.
    if (!CloseHandle(handle)) {
        throw std::system_error(
            GetLastError(), std::system_category(), "CloseHandle failed");
    }
#else
    // Release ownership of the file descriptor.
    auto fd = derived().release();

    // Close the file descriptor,
    if (-1 == ::close(fd)) {
        throw std::system_error(
            errno, std::system_category(), "close failed");
    }
#endif
}

template <typename File>
void basic_file_base<File>::sync() const
{
#ifdef ZPP_FILE_WINDOWS
    if (!FlushFileBuffers(derived().get())) {
        throw std::system_error(GetLastError(),
                                std::system_category(),
                                "FlushFileBuffers failed");
    }
#else
    if (!fsync(derived().get())) {
        throw std::system_error(
            errno, std::system_category(), "fsync failed");
    }
#endif
}

template <typename File>
void basic_file_base<File>::truncate(std::uint64_t size) const
{
#ifdef ZPP_FILE_WINDOWS
    // Seek to the end of the file.
    auto previous_position = seek(static_cast<std::int64_t>(size));

    // Set this point as the end of file.
    if (!SetEndOfFile(derived().get())) {
        throw std::system_error(
            GetLastError(), std::system_category(), "SetEndOfFile failed");
    }

    // Seek to the previous location.
    seek(previous_position);
#else
    if (-1 == ftruncate(derived().get(), size)) {
        throw std::system_error(
            errno, std::system_category(), "ftruncate failed");
    }
#endif
}

template <typename File>
std::uint64_t basic_file_base<File>::size() const
{
#ifdef ZPP_FILE_WINDOWS
    LARGE_INTEGER result{};

    // Get the file size.
    if (!GetFileSizeEx(derived().get(), &result)) {
        throw std::system_error(GetLastError(),
                                std::system_category(),
                                "GetFileSizeEx failed");
    }

    // Return the result.
    return static_cast<std::uint64_t>(result.QuadPart);
#else
    struct stat file_stat = {};

    // Query the file stat for the size.
    if (-1 == fstat(derived().get(), &file_stat)) {
        throw std::system_error(
            errno, std::system_category(), "fstat failed");
    }

    // Return the size.
    return file_stat.st_size;
#endif
}

template <typename File>
std::uint64_t basic_file_base<File>::seek(std::int64_t offset,
                                          seek_mode mode) const
{
#ifdef ZPP_FILE_WINDOWS
    LARGE_INTEGER file_pointer{};
    file_pointer.QuadPart = offset;

    // Determine the seek move method.
    DWORD move_method{};
    switch (mode) {
    case seek_mode::set:
        move_method = FILE_BEGIN;
        break;
    case seek_mode::end:
        move_method = FILE_END;
        break;
    case seek_mode::current:
        move_method = FILE_CURRENT;
        break;
    }

    // Seek the file.
    auto result = SetFilePointerEx(
        derived().get(), file_pointer, &file_pointer, move_method);

    // If failed, throw an error.
    if (!result) {
        throw std::system_error(GetLastError(),
                                std::system_category(),
                                "SetFilePointerEx failed");
    }

    return file_pointer.QuadPart;
#else
    // Determine the seek whence.
    int whence{};
    switch (mode) {
    case seek_mode::set:
        whence = SEEK_SET;
        break;
    case seek_mode::end:
        whence = SEEK_END;
        break;
    case seek_mode::current:
        whence = SEEK_CUR;
        break;
    }

    // Seek the file.
    auto result = lseek(derived().get(), offset, whence);

    // If failed, throw an error.
    if (-1 == result) {
        throw std::system_error(
            errno, std::system_category(), "lseek failed");
    }

    return result;
#endif
}

template <typename File>
std::uint64_t basic_file_base<File>::tell() const
{
#ifdef ZPP_FILE_WINDOWS
    LARGE_INTEGER file_pointer{};

    // Seek file to current position to get it.
    auto result = SetFilePointerEx(
        derived().get(), file_pointer, &file_pointer, FILE_CURRENT);

    // If failed, throw an error.
    if (!result) {
        throw std::system_error(GetLastError(),
                                std::system_category(),
                                "SetFilePointerEx failed");
    }

    return file_pointer.QuadPart;
#else
    // Seek file to current position to get it.
    auto result = lseek(derived().get(), 0, SEEK_CUR);

    // If failed, throw an error.
    if (-1 == result) {
        throw std::system_error(
            errno, std::system_category(), "lseek failed");
    }

    return result;
#endif
}

} // namespace detail

/**
 * Open a file by path forwarding parameters to underlying platform
 * specific API.
 */
template <typename Char, typename... Arguments>
inline file open(const Char * path, Arguments... arguments)
{
#ifdef ZPP_FILE_WINDOWS
    HANDLE result{};

    // Create the file.
    if constexpr (std::is_same_v<char, Char>) {
        result = CreateFileA(path, arguments...);
    } else {
        result = CreateFileW(path, arguments...);
    }

    // If file creation failed, throw an error.
    if (INVALID_HANDLE_VALUE == result) {
        throw std::system_error(
            GetLastError(), std::system_category(), "CreateFile failed");
    }

    // Return the file handle.
    return file_handle(result);
#else
    // Open the file.
    auto result = ::open(path, arguments...);

    // If open failed, throw an error.
    if (-1 == result) {
        throw std::system_error(
            errno, std::system_category(), "open failed");
    }

    // Return the file descriptor.
    return file_handle(result);
#endif
}

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
inline file open(const Char * path, open_mode mode)
{
    static_assert(std::is_same_v<Char, char> ||
                      std::is_same_v<Char, wchar_t>,
                  "Invalid path type.");

#ifdef ZPP_FILE_WINDOWS
    DWORD desired_access{};
    DWORD creation_disposition{};
    bool append = false;

    // Get create file desired access, creation disposition, and append.
    switch (mode) {
    case open_mode::read:
        desired_access = GENERIC_READ;
        creation_disposition = OPEN_EXISTING;
        break;
    case open_mode::write:
        desired_access = GENERIC_WRITE;
        creation_disposition = CREATE_ALWAYS;
        break;
    case open_mode::append:
        desired_access = GENERIC_WRITE;
        creation_disposition = OPEN_ALWAYS;
        append = true;
        break;
    case open_mode::read_write:
        desired_access = GENERIC_READ | GENERIC_WRITE;
        creation_disposition = OPEN_EXISTING;
        break;
    case open_mode::read_write_create:
        desired_access = GENERIC_READ | GENERIC_WRITE;
        creation_disposition = CREATE_ALWAYS;
        break;
    case open_mode::read_write_append:
        desired_access = GENERIC_READ | GENERIC_WRITE;
        creation_disposition = OPEN_ALWAYS;
        append = true;
        break;
    }

    // Create the file.
    HANDLE result{};
    if constexpr (std::is_same_v<char, Char>) {
        result = CreateFileA(path,
                             desired_access,
                             FILE_SHARE_READ | FILE_SHARE_WRITE |
                                 FILE_SHARE_DELETE,
                             nullptr,
                             creation_disposition,
                             FILE_ATTRIBUTE_NORMAL,
                             nullptr);
    } else {
        result = CreateFileW(path,
                             desired_access,
                             FILE_SHARE_READ | FILE_SHARE_WRITE |
                                 FILE_SHARE_DELETE,
                             nullptr,
                             creation_disposition,
                             FILE_ATTRIBUTE_NORMAL,
                             nullptr);
    }

    // If file creation failed, throw an error.
    if (INVALID_HANDLE_VALUE == result) {
        throw std::system_error(
            GetLastError(), std::system_category(), "CreateFile failed");
    }

    // Create the file.
    file file = file_handle(result);

    // If append, seek to the end.
    if (append) {
        file.seek(0, seek_mode::end);
    }

    // Return the file.
    return file;
#else
    // Get open flags.
    int flags{};
    switch (mode) {
    case open_mode::read:
        flags = O_RDONLY;
        break;
    case open_mode::write:
        flags = O_WRONLY | O_CREAT | O_TRUNC;
        break;
    case open_mode::append:
        flags = O_WRONLY | O_CREAT | O_APPEND;
        break;
    case open_mode::read_write:
        flags = O_RDWR;
        break;
    case open_mode::read_write_create:
        flags = O_RDWR | O_CREAT | O_TRUNC;
        break;
    case open_mode::read_write_append:
        flags = O_RDWR | O_CREAT | O_APPEND;
        break;
    }

    // Open the file.
    auto result = ::open(path, flags);

    // If open failed, throw an error.
    if (-1 == result) {
        throw std::system_error(
            errno, std::system_category(), "open failed");
    }

    // Return the file descriptor.
    return file_handle(result);
#endif
}

} // namespace zpp::filesystem

#ifdef ZPP_FILE_WINDOWS
#undef ZPP_FILE_WINDOWS
#endif
