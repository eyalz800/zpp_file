#pragma once

#include <algorithm>
#include <cstddef>
#include <cstdint>
#include <iterator>
#include <limits>
#include <memory>
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

namespace zpp
{
#if !__has_include("zpp/byte_view.h")
/**
 * Represents a view of bytes.
 */
template <typename ByteType>
class basic_byte_view
{
public:
    // Check that the underlying type is either char, unsigned char, or
    // std::byte.
    static_assert(
        std::is_same_v<std::remove_cv_t<ByteType>, char> ||
            std::is_same_v<std::remove_cv_t<ByteType>, unsigned char> ||
            std::is_same_v<std::remove_cv_t<ByteType>, std::byte>,
        "Byte type must either be char, unsigned char, or std::byte.");
    /**
     * Type definition.
     * @{
     */
    using value_type = ByteType;
    using const_value_type = std::add_const_t<value_type>;
    using reference = std::add_lvalue_reference_t<value_type>;
    using const_reference = std::add_lvalue_reference_t<const_value_type>;
    using pointer = std::add_pointer_t<value_type>;
    using const_pointer = std::add_pointer_t<const_value_type>;
    using iterator = pointer;
    using const_iterator = const_pointer;
    using reverse_iterator = std::reverse_iterator<iterator>;
    using const_reverse_iterator = std::reverse_iterator<const_iterator>;
    using index_type = std::size_t;
    /**
     * @}
     */

    /**
     * Construct an empty byte view.
     */
    constexpr basic_byte_view() noexcept = default;

    /**
     * Construction of byte view for same byte ranges.
     * @{
     */
    constexpr basic_byte_view(pointer begin, index_type count) noexcept :
        m_data(begin),
        m_size(count)
    {
    }

    constexpr basic_byte_view(pointer begin, pointer end) noexcept :
        basic_byte_view(begin, end - begin)
    {
    }
    /**
     * @}
     */

    /**
     * Construction of byte view from different byte ranges.
     * @{
     */
    template <typename Pointer,
              // Make sure other pointer is not pointing to the same type
              // current byte view type.
              typename = std::enable_if_t<!std::is_same_v<
                  std::remove_cv_t<std::remove_pointer_t<Pointer>>,
                  std::remove_cv_t<value_type>>>,

              // Make sure the other pointer points to a byte type.
              typename = std::enable_if_t<
                  std::is_same_v<
                      std::remove_cv_t<std::remove_pointer_t<Pointer>>,
                      char> ||
                  std::is_same_v<
                      std::remove_cv_t<std::remove_pointer_t<Pointer>>,
                      unsigned char> ||
                  std::is_same_v<
                      std::remove_cv_t<std::remove_pointer_t<Pointer>>,
                      std::byte>>>
    basic_byte_view(Pointer begin, index_type count) noexcept :
        m_data(reinterpret_cast<pointer>(begin)),
        m_size(count)
    {
    }

    template <typename Pointer,
              // Make sure other pointer is not pointing to the same type
              // current byte view type.
              typename = std::enable_if_t<!std::is_same_v<
                  std::remove_cv_t<std::remove_pointer_t<Pointer>>,
                  std::remove_cv_t<value_type>>>,

              // Make sure the other pointer points to a byte type.
              typename = std::enable_if_t<
                  std::is_same_v<
                      std::remove_cv_t<std::remove_pointer_t<Pointer>>,
                      char> ||
                  std::is_same_v<
                      std::remove_cv_t<std::remove_pointer_t<Pointer>>,
                      unsigned char> ||
                  std::is_same_v<
                      std::remove_cv_t<std::remove_pointer_t<Pointer>>,
                      std::byte>>>
    basic_byte_view(Pointer begin, Pointer end) noexcept :
        basic_byte_view(begin, end - begin)
    {
    }
    /**
     * @}
     */

    /**
     * Explicit construction of byte views of non-byte ranges.
     * @{
     */
    template <typename Pointer,
              // Make sure the other pointer is not a byte type.
              typename = std::enable_if_t<
                  !std::is_same_v<
                      std::remove_cv_t<std::remove_pointer_t<Pointer>>,
                      char> &&
                  !std::is_same_v<
                      std::remove_cv_t<std::remove_pointer_t<Pointer>>,
                      unsigned char> &&
                  !std::is_same_v<
                      std::remove_cv_t<std::remove_pointer_t<Pointer>>,
                      std::byte>>>
    explicit basic_byte_view(Pointer begin, index_type count) noexcept :
        m_data(reinterpret_cast<pointer>(begin)),
        m_size(count * sizeof(*begin))
    {
    }

    template <typename Pointer,
              // Make sure the other pointer is not a byte type.
              typename = std::enable_if_t<
                  !std::is_same_v<
                      std::remove_cv_t<std::remove_pointer_t<Pointer>>,
                      char> &&
                  !std::is_same_v<
                      std::remove_cv_t<std::remove_pointer_t<Pointer>>,
                      unsigned char> &&
                  !std::is_same_v<
                      std::remove_cv_t<std::remove_pointer_t<Pointer>>,
                      std::byte>>>
    explicit basic_byte_view(Pointer begin, Pointer end) noexcept :
        basic_byte_view(begin, end - begin)
    {
    }
    /**
     * @}
     */

    /**
     * Explicit construction of byte views from void pointer ranges.
     * @{
     */
    template <typename Pointer,
              // Make sure the other pointer is void type.
              typename = std::enable_if_t<std::is_void_v<
                  std::remove_cv_t<std::remove_pointer_t<Pointer>>>>,
              typename = void,
              typename = void>
    explicit basic_byte_view(Pointer begin, index_type count) noexcept :
        m_data(static_cast<pointer>(begin)),
        m_size(count)
    {
    }

    template <typename Pointer,
              // Make sure the other pointer is void type.
              typename = std::enable_if_t<std::is_void_v<
                  std::remove_cv_t<std::remove_pointer_t<Pointer>>>>,
              typename = void,
              typename = void,
              typename = void>
    explicit basic_byte_view(Pointer begin, Pointer end) noexcept :
        basic_byte_view(
            begin, Pointer(std::uintptr_t(end) - std::uintptr_t(begin)))
    {
    }
    /**
     * @}
     */

    /**
     * Construction from other byte views whose underlying type is
     * convertible (CV qualification).
     * @{
     */
    template <typename OtherByteType,
              typename = std::enable_if_t<
                  std::is_convertible_v<std::add_pointer_t<OtherByteType>,
                                        pointer>>>
    constexpr basic_byte_view(
        const basic_byte_view<OtherByteType> & other) noexcept :
        m_data(other.m_data),
        m_size(other.m_size)
    {
    }

    template <typename OtherByteType,
              typename = std::enable_if_t<
                  std::is_convertible_v<std::add_pointer_t<OtherByteType>,
                                        pointer>>>
    constexpr basic_byte_view(
        basic_byte_view<OtherByteType> && other) noexcept :
        m_data(other.m_data),
        m_size(other.m_size)
    {
    }
    /**
     * @}
     */

    /**
     * Construct from other byte views with non convertible underlying
     * type.
     */
    template <typename OtherByteType,
              typename = std::enable_if_t<
                  !std::is_convertible_v<std::add_pointer_t<OtherByteType>,
                                         pointer>>,
              typename = void>
    basic_byte_view(
        const basic_byte_view<OtherByteType> & other) noexcept :
        m_data(reinterpret_cast<pointer>(other.m_data)),
        m_size(other.m_size)
    {
    }

    /**
     * Construct from containers/string/array of byte types.
     * @{
     */
    template <typename Type,
              // The pointer type.
              typename Pointer = decltype(std::data(std::declval<Type>())),

              // Make sure the container is random access (that together
              // with std::data() function is probably enough to
              // require contiguous).
              typename = std::enable_if_t<std::is_base_of_v<
                  std::random_access_iterator_tag,
                  typename std::iterator_traits<decltype(std::begin(
                      std::declval<Type>()))>::iterator_category>>,

              // Make sure other pointer is pointing to the same type
              // current byte view type.
              typename = std::enable_if_t<std::is_same_v<
                  std::remove_cv_t<std::remove_pointer_t<Pointer>>,
                  std::remove_cv_t<value_type>>>,

              // Make sure the other pointer points to a byte type.
              typename = std::enable_if_t<
                  std::is_same_v<
                      std::remove_cv_t<std::remove_pointer_t<Pointer>>,
                      char> ||
                  std::is_same_v<
                      std::remove_cv_t<std::remove_pointer_t<Pointer>>,
                      unsigned char> ||
                  std::is_same_v<
                      std::remove_cv_t<std::remove_pointer_t<Pointer>>,
                      std::byte>>>
    constexpr basic_byte_view(Type && value) noexcept :
        basic_byte_view(std::data(value), std::size(value))
    {
    }

    template <typename Type,
              // The pointer type.
              typename Pointer = decltype(std::data(std::declval<Type>())),

              // Make sure the container is random access (that together
              // with std::data() function is probably enough to
              // require contiguous).
              typename = std::enable_if_t<std::is_base_of_v<
                  std::random_access_iterator_tag,
                  typename std::iterator_traits<decltype(std::begin(
                      std::declval<Type>()))>::iterator_category>>,

              // Make sure other pointer is not pointing to the same type
              // current byte view type.
              typename = std::enable_if_t<!std::is_same_v<
                  std::remove_cv_t<std::remove_pointer_t<Pointer>>,
                  std::remove_cv_t<value_type>>>,

              // Make sure the other pointer points to a byte type.
              typename = std::enable_if_t<
                  std::is_same_v<
                      std::remove_cv_t<std::remove_pointer_t<Pointer>>,
                      char> ||
                  std::is_same_v<
                      std::remove_cv_t<std::remove_pointer_t<Pointer>>,
                      unsigned char> ||
                  std::is_same_v<
                      std::remove_cv_t<std::remove_pointer_t<Pointer>>,
                      std::byte>>,
              typename = void>
    basic_byte_view(Type && value) noexcept :
        basic_byte_view(std::data(value), std::size(value))
    {
    }
    /**
     * @}
     */

    /**
     * Explicitly construct from containers/string/array of non byte types.
     * @{
     */
    template <typename Type,
              // The pointer type.
              typename Pointer = decltype(std::data(std::declval<Type>())),

              // Make sure the container is random access (that together
              // with a std::data() function is probably enough to
              // require contiguous).
              typename = std::enable_if_t<std::is_base_of_v<
                  std::random_access_iterator_tag,
                  typename std::iterator_traits<decltype(std::begin(
                      std::declval<Type>()))>::iterator_category>>,

              // Make sure the other pointer does not point to a byte type.
              typename = std::enable_if_t<
                  !std::is_same_v<
                      std::remove_cv_t<std::remove_pointer_t<Pointer>>,
                      char> &&
                  !std::is_same_v<
                      std::remove_cv_t<std::remove_pointer_t<Pointer>>,
                      unsigned char> &&
                  !std::is_same_v<
                      std::remove_cv_t<std::remove_pointer_t<Pointer>>,
                      std::byte>>>
    explicit basic_byte_view(Type && value) noexcept :
        basic_byte_view(std::data(value), std::size(value))
    {
    }
    /**
     * @}
     */

    /**
     * Assignment and swap.
     * @{
     */
    constexpr basic_byte_view & operator=(basic_byte_view other) noexcept
    {
        other.swap(*this);
        return *this;
    }
    constexpr void swap(basic_byte_view & other) noexcept
    {
        std::swap(m_data, other.m_data);
        std::swap(m_size, other.m_size);
    }
    /**
     * @}
     */

    /**
     * Accessing a byte at a specific index.
     * @{
     */
    constexpr reference operator[](index_type index)
    {
        return m_data[index];
    }
    constexpr reference operator[](index_type index) const
    {
        return m_data[index];
    }
    reference at(index_type index)
    {
        if (index >= m_size) {
            throw std::out_of_range("byte view access out of range");
        }
        return m_data[index];
    }
    reference at(index_type index) const
    {
        if (index >= m_size) {
            throw std::out_of_range("byte view access out of range");
        }
        return m_data[index];
    }
    /**
     * @}
     */

    /**
     * Accessing front and back.
     * @{
     */
    constexpr reference front()
    {
        return *m_data;
    }
    constexpr reference front() const
    {
        return *m_data;
    }
    constexpr reference back()
    {
        return *(m_data + m_size - 1);
    }
    constexpr reference back() const
    {
        return *(m_data + m_size - 1);
    }
    /**
     * @}
     */

    /**
     * Return the byte view data and size.
     * @{
     */
    constexpr pointer data()
    {
        return m_data;
    }
    constexpr pointer data() const
    {
        return m_data;
    }
    constexpr index_type size() const
    {
        return m_size;
    }
    constexpr bool empty() const
    {
        return !m_size;
    }
    /**
     * @}
     */

    /**
     * Iteration support.
     * @{
     */
    constexpr iterator begin()
    {
        return m_data;
    }
    constexpr iterator begin() const
    {
        return m_data;
    }
    constexpr const_iterator cbegin() const
    {
        return m_data;
    }
    constexpr reverse_iterator rbegin()
    {
        return reverse_iterator(end());
    }
    constexpr const_reverse_iterator rbegin() const
    {
        return const_reverse_iterator(end());
    }
    constexpr const_reverse_iterator crbegin() const
    {
        return const_reverse_iterator(end());
    }
    constexpr iterator end()
    {
        return m_data + m_size;
    }
    constexpr iterator end() const
    {
        return m_data + m_size;
    }
    constexpr const_iterator cend() const
    {
        return m_data + m_size;
    }
    constexpr reverse_iterator rend()
    {
        return reverse_iterator(begin());
    }
    constexpr reverse_iterator rend() const
    {
        return reverse_iterator(begin());
    }
    constexpr const_reverse_iterator crend() const
    {
        return reverse_iterator(begin());
    }
    /**
     * @}
     */

private:
    /**
     * Pointer to the byte view beginning.
     */
    pointer m_data{};

    /**
     * Size of the byte view.
     */
    index_type m_size{};
};

/**
 * Swap between left and right byte views.
 */
template <typename Type>
constexpr void swap(basic_byte_view<Type> & left,
                    basic_byte_view<Type> & right) noexcept
{
    left.swap(right);
}

/**
 * Byte view aliases.
 * @{
 */
using byte_view = basic_byte_view<std::byte>;
using cbyte_view = basic_byte_view<const std::byte>;
/**
 * @}
 */
} // namespace zpp
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
     * Reads all requested bytes, unless the end of file is reached where
     * the reading stops. Returns the data inside a vector of bytes.
     */
    std::vector<std::byte> read(std::size_t size) const;

    /**
     * Reads the entire file and returns its data.
     */
    std::vector<std::byte> read() const;

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
     * Creates an insufficient space exception.
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
     * Become friends with all files.
     */
    template <typename, typename>
    friend class basic_file;

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
     * Create a file from another file if file handle is convertible.
     */
    template <typename OtherFileHandle>
    basic_file(
        const basic_file<OtherFileHandle, InvalidFileHandle> & other) :
        m_file(other.m_file)
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
     * Returns the weak file handle.
     */
    explicit operator weak_file_handle() const
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
std::size_t basic_file_base<File>::read(byte_view data) const
{
    std::size_t bytes_read{};

    while (data.size() > bytes_read) {
        // Perform a single read.
        auto result = read_once(
            {data.data() + bytes_read, data.size() - bytes_read});

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
std::size_t basic_file_base<File>::write(cbyte_view data) const
{
    std::size_t bytes_written{};

    while (data.size() > bytes_written) {
        // Perform a single write.
        auto result = write_once(
            {data.data() + bytes_written, data.size() - bytes_written});

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
std::vector<std::byte> basic_file_base<File>::read(std::size_t size) const
{
    // The file data.
    std::vector<std::byte> data(size);

    // Perform the read operation and resize accordingly.
    data.resize(read(data));

    // Shrink the data if needed, and return.
    data.shrink_to_fit();
    return data;
}

template <typename File>
std::vector<std::byte> basic_file_base<File>::read() const
{
    // In the absence of size and file size, this will be the initial size
    // of the vector.
    constexpr std::size_t initial_vector_size = 0x1000;

    // The file data.
    std::vector<std::byte> data;

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

    // The amount of bytes read.
    std::size_t bytes_read{};

    // Read the data.
    while (true) {
        // Compute the amount of bytes to read.
        std::size_t bytes_to_read = data.size() - bytes_read;

        // Perform the read operation.
        auto result = read({data.data() + bytes_read, bytes_to_read});

        // Update the bytes read.
        bytes_read += result;

        // If we read less than requested, the end of file is reached.
        if (result < bytes_to_read) {
            data.resize(bytes_read);
            break;
        }

        // If data size is already the maximum size, throw an error.
        if (data.size() == (std::numeric_limits<std::size_t>::max)()) {
            throw std::range_error("Amount of file data is too "
                                   "large for this platform.");
        }

        // The new size.
        std::size_t new_size{};

        // Limit to resizing by 2/3 factor.
        constexpr auto resize_factor_limit =
            (std::numeric_limits<std::size_t>::max)() / 3 * 2;

        // If data size is below resize by factor limit, resize
        // according to the factor, otherwise, resize to max.
        if (data.size() < resize_factor_limit) {
            new_size = data.size() / 2 * 3;
        } else {
            new_size = (std::numeric_limits<std::size_t>::max)();
        }

        // Resize the vector.
        data.resize(new_size);
        continue;
    }

    // Shrink the data if needed, and return.
    data.shrink_to_fit();
    return data;
}

template <typename File>
void basic_file_base<File>::read_exact(byte_view data) const
{
    if (auto result = read(data); result != data.size()) {
        throw end_of_file_exception(result);
    }
}

template <typename File>
void basic_file_base<File>::write_exact(cbyte_view data) const
{
    if (auto result = write(data); result != data.size()) {
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
std::size_t basic_file_base<File>::read_once(byte_view data) const
{
#ifdef ZPP_FILE_WINDOWS
    // The maximum bytes windows can read at once.
    static constexpr std::size_t max_read_size =
        (std::numeric_limits<DWORD>::max)();

    // The amount of bytes to read at this time.
    auto bytes_to_read =
        static_cast<DWORD>((std::min)(data.size(), max_read_size));

    // The bytes read.
    DWORD bytes_read{};

    // Read from the file.
    auto result = ReadFile(
        derived().get(), data.data(), bytes_to_read, &bytes_read, nullptr);

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
        result = ::read(derived().get(), data.data(), data.size());
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
std::size_t basic_file_base<File>::write_once(cbyte_view data) const
{
#ifdef ZPP_FILE_WINDOWS
    // The maximum bytes windows can write at once.
    static constexpr std::size_t max_write_size =
        (std::numeric_limits<DWORD>::max)();

    // The amount of bytes to write at this time.
    auto bytes_to_write =
        static_cast<DWORD>((std::min)(data.size(), max_write_size));

    // The bytes written.
    DWORD bytes_written{};

    // Write to the file.
    auto result = WriteFile(derived().get(),
                            data.data(),
                            bytes_to_write,
                            &bytes_written,
                            nullptr);

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
        result = ::write(derived().get(), data.data(), data.size());
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

    // Close the handle handle.
    if (!CloseHandle(handle)) {
        throw std::system_error(
            GetLastError(), std::system_category(), "CloseHandle failed");
    }
#else
    // Release ownership of the file descriptor.
    auto fd = derived().release();

    // Close the local file descriptor,
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
file open(const Char * path, Arguments... arguments)
{
    static_assert(std::is_same_v<Char, char> ||
                      std::is_same_v<Char, wchar_t>
#ifdef __cpp_char8_t
                      || std::is_same_v<Char, char8_t>
#endif
                  ,
                  "Invalid path type.");

#ifdef ZPP_FILE_WINDOWS
    HANDLE result{};

    // Create file for utf8 strings.
    auto create_file_utf8 = [](auto path, auto... arguments) {
        // The path size, including the null terminator.
        auto path_size = strlen(path) + 1;

        // Check needed size for the wide path.
        auto wide_path_size = MultiByteToWideChar(
            CP_UTF8, 0, path, static_cast<int>(path_size), nullptr, 0);
        if (!wide_path_size) {
            throw std::system_error(GetLastError(),
                                    std::system_category(),
                                    "MultiByteToWideChar failed");
        }

        // Create the wide path.
        auto wide_path = std::make_unique<wchar_t[]>(wide_path_size);

        // Convert the utf8 path into wide path.
        if (!MultiByteToWideChar(CP_UTF8,
                                 0,
                                 path,
                                 static_cast<int>(path_size),
                                 wide_path.get(),
                                 wide_path_size)) {
            throw std::system_error(GetLastError(),
                                    std::system_category(),
                                    "MultiByteToWideChar failed");
        }

        // Create the file using the wide path.
        return CreateFileW(wide_path.get(), arguments...);
    };

    // If character type is char, open using ASCII or UTF8 depending on the
    // settings.
    if constexpr (std::is_same_v<Char, char>) {
#ifndef ZPP_FILE_OPEN_CHAR_IS_UTF8
        result = CreateFileA(path, arguments...);
#else
        result = create_file_utf8(path, arguments...);
#endif
#ifdef __cpp_char8_t
    } else if constexpr (std::is_same_v<Char, char8_t>) {
        result = create_file_utf8(reinterpret_cast<const char *>(path),
                                  arguments...);
#endif
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
    int result{};

    // Open the file.
#ifdef __cpp_char8_t
    if constexpr (std::is_same_v<Char, char8_t>) {
        result =
            ::open(reinterpret_cast<const char *>(path), arguments...);
    } else {
#endif
        result = ::open(path, arguments...);
#ifdef __cpp_char8_t
    }
#endif

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
file open(const Char * path, open_mode mode)
{
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

    // Open the file.
    auto file =
        open(path,
             desired_access,
             FILE_SHARE_READ | FILE_SHARE_WRITE | FILE_SHARE_DELETE,
             nullptr,
             creation_disposition,
             FILE_ATTRIBUTE_NORMAL,
             nullptr);

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
    return open(path, flags | O_CLOEXEC);
#endif
}

/**
 * Open by string views.
 * @{
 */
template <typename... Arguments>
file open(std::string_view path, Arguments &&... arguments)
{
    return open(path.data(), arguments...);
}

inline file open(std::string_view path, open_mode mode)
{
    return open(path.data(), mode);
}

#ifdef __cpp_lib_char8_t
template <typename... Arguments>
file open(std::u8string_view path, Arguments &&... arguments)
{
    return open(path.data(), arguments...);
}

inline file open(std::u8string_view path, open_mode mode)
{
    return open(path.data(), mode);
}
#endif

#ifdef ZPP_FILE_WINDOWS
template <typename... Arguments>
file open(std::wstring_view path, Arguments &&... arguments)
{
    return open(path.data(), arguments...);
}

inline file open(std::wstring_view path, open_mode mode)
{
    return open(path.data(), mode);
}
#endif
/**
 * @}
 */

} // namespace zpp::filesystem

#ifdef ZPP_FILE_WINDOWS
#undef ZPP_FILE_WINDOWS
#endif
