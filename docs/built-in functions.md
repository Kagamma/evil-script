## Table of Contents
- [Base64](#base64)
- [Buffers](#buffers)
- [Coroutines](#coroutines)
- [Datetime](#datetime)
- [File system](#file-system)
- [JSON](#json)
- [Math](#math)
- [Map](#map)
- [Memory management](#memory-management)
- [Misc](#misc)
- [Multithreading](#multithreading)
- [Strings](#strings)

## Base64
`base64_encode(buf: buffer): string`
- Encodes a given string to base64 string.

`base64_decode(s: string): buffer`
- Decodes a given base64 string back to original string.

## Buffers
`buffer_create(size: number): buffer`
- Creates a new buffer. The result is a pointer points to the start of allocated memory.

`buffer_length(buffer: buffer): number`
- Returns length of a buffer.

`buffer_copy(dst, src: buffer, count: number): number`
- Copy `count` bytes from `src` to `dst`.

`buffer_u8_fill(buffer: buffer, value, count: number): number`
- Sets the first `count` bytes of the block of memory pointed by `buffer` to the specified value (interpreted as an unsigned char).

`buffer_u16_fill(buffer: buffer, value, count: number): number`
- Sets the first `count ` 2 bytes of the block of memory pointed by `buffer` to the specified value (interpreted as an unsigned short).

`buffer_u32_fill(buffer: buffer, value, count: number): number`
- Sets the first `count ` 4 bytes of the block of memory pointed by `buffer` to the specified value (interpreted as an unsigned int).

`buffer_u64_fill(buffer: buffer, value, count: number): number`
- Sets the first `count ` 8 bytes of the block of memory pointed by `buffer` to the specified value (interpreted as an unsigned long long).

`buffer_i8_fill(buffer: buffer, value, count: number): number`
- Sets the first `count` bytes of the block of memory pointed by `buffer` to the specified value (interpreted as an char).

`buffer_i16_fill(buffer: buffer, value, count: number): number`
- Sets the first `count ` 2 bytes of the block of memory pointed by `buffer` to the specified value (interpreted as an short).

`buffer_i32_fill(buffer: buffer, value, count: number): number`
- Sets the first `count ` 4 bytes of the block of memory pointed by `buffer` to the specified value (interpreted as an int).

`buffer_i64_fill(buffer: buffer, value, count: number): number`
- Sets the first `count ` 8 bytes of the block of memory pointed by `buffer` to the specified value (interpreted as an long long).

`buffer_f32_fill(buffer: buffer, value, count: number): number`
- Sets the first `count ` 4 bytes of the block of memory pointed by `buffer` to the specified value (interpreted as an float).

`buffer_f64_fill(buffer: buffer, value, count: number): number`
- Sets the first `count ` 8 bytes of the block of memory pointed by `buffer` to the specified value (interpreted as an double).

`buffer_u8_get(buffer: buffer): number`
- Gets 1-byte unsigned data from buffer.

`buffer_i8_get(buffer: buffer): number`
- Gets 1-byte data from buffer.

`buffer_u16_get(buffer: buffer): number`
- Gets 2-byte unsigned data from buffer.

`buffer_i16_get(buffer: buffer): number`
- Gets 2-byte data from buffer.

`buffer_u32_get(buffer: buffer): number`
- Gets 4-byte unsigned data from buffer.

`buffer_i32_get(buffer: buffer): number`
- Gets 4-byte data from buffer.

`buffer_u64_get(buffer: buffer): number`
- Gets 8-byte unsigned data from buffer.

`buffer_i64_get(buffer: buffer): number`
- Gets 8-byte data from buffer.

`buffer_f32_get(buffer: buffer): number`
- Gets float-type data from buffer.

`buffer_f64_get(buffer: buffer): number`
- Gets double-type data from buffer.

`buffer_u8_set(buffer: buffer, data: number): number`
- Writes 1-byte unsigned data to buffer.

`buffer_i8_set(buffer: buffer, data: number): number`
- Writes 1-byte data to buffer.

`buffer_u16_set(buffer: buffer, data: number): number`
- Writes 2-byte unsigned data to buffer.

`buffer_i16_set(buffer: buffer, data: number): number`
- Writes 2-byte data to buffer.

`buffer_u32_set(buffer: buffer, data: number): number`
- Writes 4-byte unsigned data to buffer.

`buffer_i32_set(buffer: buffer, data: number): number`
- Writes 4-byte data to buffer.

`buffer_u64_set(buffer: buffer, data: number): number`
- Writes 8-byte unsigned data to buffer.

`buffer_i64_set(buffer: buffer, data: number): number`
- Writes 8-byte data to buffer.

`buffer_f32_set(buffer: buffer, data: number): number`
- Writes float-type data to buffer.

`buffer_f64_set(buffer: buffer, data: number): number`
- Writes double-type data to buffer.

`string_to_buffer(s: string): buffer`
- Returns pointer point to the first element of the string.

`buffer_to_string(b: buffer): string`
- Copies buffer content to string.

`wbuffer_to_string(b: buffer): string`
- Copies wbuffer content to string.

`buffer_to_array_f32(b: buffer, count: number): map`
- Converts `count` floats from buffer to valid array.

`buffer_to_array_f64(b: buffer, count: number): map`
- Converts `count` doubles from buffer to valid array.

`array_to_buffer_f32(a: map): buffer`
- Converts a valid array to a buffer. The buffer's type of data is float.

`array_to_buffer_f64(a: map): buffer`
- Converts a valid array to a buffer. The buffer's type of data is double.

## Coroutines
`coroutine_create(function, any...): pasobject`
- Creates a new coroutine with the provided function and returns coroutine's pasobject instance. The first argument must be a function, any additional arguments will be passed to that function when the coroutine starts. The coroutine's pasobject instance is passed to the function as `self`.

`coroutine_start(pasobject): any`
- Begin or resume coroutine execution. Returns value set by either `yield` or `result`.

`coroutine_resume(pasobject): any`
- Alias to `coroutine_start`.

`coroutine_is_terminated(pasobject): boolean`
- Check if coroutine's `terminated` flag is set. Returns `true` if the flag is set.

`coroutine_terminate(pasobject)`
- Set the `terminated` flag of the coroutine to `true`.

`coroutine_is_running(pasobject): boolean`
- Returns `true` if the coroutine is running. Useful to detect if a function is executing inside a coroutine.

## Datetime
`ticks(): number`
- Returns system's ticks, in miliseconds.

`dt_now(): number`
- Returns current time in datetime format.

`dt_year_get(dt: number): number`
- Returns year in number.

`dt_month_get(dt: number): number`
- Returns month number.

`dt_day_get(dt: number): number: number`
- Returns day number.

`dt_hour_get(dt: number): number`
- Returns hour number.

`dt_minute_get(dt: number): number`
- Returns minute number.

`dt_day_add(dt, days: number): number`
- Increases dt by number of days.

`dt_month_add(dt, months: number): number`
- Increases dt by number of months.

`dt_year_add(dt, years: number): number`
- Increases dt by number of years.

`dt_date_set(year, month, day: number): number`
- Encodes date from year, month and day.

`dt_time_set(hour, minute, second, milisecond: number): number`
- Encodes time from hour, minute, second and milisecond.

## File system
`fs_directory_create(path: string)`
- Creates new directory.

`fs_directory_delete(path: string)`
- Deletes directory.

`fs_directory_find_all(path: string, is_subdir: boolean)`
- Performs search for directories in certain paths. Returns map of paths.

`fs_directory_exists(path: string): boolean`.
- Checks if a directory is exists.

`fs_file_read_binary(filename: string): buffer`
- Reads content from file..

`fs_file_read_binary(filename: string; start, size: number): buffer`
- Reads content from file, starting from `start` and end with `start + size`. Returns `null` if no data can be read from file.

`fs_file_write_binary(filename: string, buf: buffer, buf_size: number)`
- Writes content at the end file. If the file is not exist then a new file is created.

`fs_file_read_text(filename: string): string`
- Reads text from file.

`fs_file_write_text(filename, text: string)`
- Writes text at the end of file. If the file is not exist then a new file is created.
`fs_file_find_all(path, mask: string, is_subdir: boolean, attribute:
number): map`
- Performs search for files in certain paths. Return map of paths.
- List of attributes:
  + FA_DIRECTORY
  + FA_READONLY
  + FA_NORMAL
  + FA_ENCRYPTED
  + FA_COMPRESSED
  + FA_SYMLINK
  + FA_SYSFILE
  + FA_ANYFILE

`fs_file_copy(src, dst: string): boolean`
- Copies src to dst, override if dst exists. Returns true if success.

`fs_file_exists(filename: string): boolean`
- Checks if a file exists.

`fs_file_delete(filename: string)`
- Deletes a file.

`fs_file_rename(oldname, newname: string)`
- Renames a file.

`fs_file_size_get(filename: string): number`
- Returns size of file in bytes.

`fs_file_age_get(filename: string): number`
- Returns the last modification Unix time of file.

## JSON
`json_parse(json: string): map`
- Converts a JSON string to map.
  + `json = json_parse('{ "a": 5, "b": 2, "c": { "d": "a text", "e": ["another text", 2] } }')` will return a map, which can be accessed for values. Example: `json.c.e[0]`

`json_stringify(map: map): string`
- Converts a map to JSON string.

## Math
`sign(n: number): number`

`round(n: number): number`

`floor(n: number): number`

`ceil(n: number): number`

`sin(n: number): number`

`cos(n: number): number`

`tan(n: number): number`

`cot(n: number): number`

`sqrt(n: number): number`

`abs(n: number): number`

`frac(n: number): number`

`range(x, y: number): map`
- Returns [x..y] array with step = 1.

`range(x, y, step: number): map`
- Returns [x..y] array.

`min(number…): number`

`max(number…): number`

`random(n: number): number`
- Returns a random integer number range from 0 - (n-1)

`rnd: number`
- Returns a random number range from 0 - 1

## Map
`map_create(): map`
Creates a new map. This function is comparable to [] syntax.

`map_key_delete(a: map, key: number/string): map`
Deletes map elements.

`map_keys_get(a: map): map`
Returns map contains all keys from map `a`.

`array_resize(a: map, size: number): map`
Resizes a valid array.

`array_to_map(arr: map): map`
Converts array `arr` to map. Note that `arr` itself will be converted.

`array_fill(arr: map, v: any): map`
Fills array `arr` with `v`.

`length(a: map/string)`
Returns length of string, map or buffer.

## Memory management
`mem_object_count(): number`
- Returns number of objects allocated by script engine.

`mem_gc()`
- Triggers garbage collection.

## Misc
`typeof(v: any): string`
- Returns a string based on the type of variable (number / boolean / string / map / array / buffer / function / pasobject / null).

`kindof(v: any): number`
- Returns a number based on the type of variable (sevkNumber / sevkBoolean / sevkString / sevkMap / sevkBuffer / sevkFunction / sevkPascalObject / sevkNull).

`string(n: number): string`
- Converts n to string.

`number(s: string): number`
- Converts s to number.

`write(any...)`
- Print text on screen. Similar to Pascal's Write().

`writeln(any...)`
- Print text on screen + newline. Similar to Pascal's Writeln().

`chr(number): string`
- Typecasts a number (0..255) to equivalent char value.

`ord(string): number`
- Typecasts a char to equivalent number value.

## Multithreading
`thread_create(function, any...): pasobject`
- Creates a new thread with the the provided function and returns thread's pasobject instance. The first argument must be a function, any additional arguments will be passed to that function when the thread starts. The thread's pasobject instance is passed to the function as `self`.

`thread_start(pasobject)`
- Begin / resume thread execution.

`thread_is_terminated(pasobject): boolean`
- Check if thread's `terminated` flag is set. Returns `true` if the flag is set.

`thread_suspend(pasobject)`
- Suspends the execution of a running thread.

`thread_resume(pasobject)`
- Alias to `thread_start`.

`thread_terminate(pasobject)`
- Set the `terminated` flag of the thread to `true`.

`thread_wait(pasobject)`
- Waits for the thread to terminate.

`critical_create(): pasobject`
- Creates a new critical section (lock) instance.

`critical_enter(pasobject)`
- Acquires the lock. Suspend the thread until the lock is acquired.

`critical_leave(pasobject)`
- Release the lock.

`critical_try(pasobject): boolean`
- Try to acquire the lock. Returns `true` if the lock is acquired.

`event_create(): pasobject`
- Creates a new event instance.

`event_set(pasobject): pasobject`
- Signal the event. Any thread that was waiting for the event to be set (using `event_wait()`) will resume it's operation.

`event_wait(pasobject)`
- Should be used in threads that should be notified when the event is set. The thread will be suspended until the event is signaled using `event_set()`.

`event_reset(pasobject)`
- Reset the event. Any threads calling `event_wait()` will be suspended.

## Strings
`string_concat(s, s1, s2: string)`
- Concatenates s1 and s2 and save result to s, without creating a new copy of s. Use this instead of `s = s1 + s2` if you try to concatenate a lot of strings.

`string_empty(s)`
- Empties string s. It is used to set a string built by /string_concat()/ back to an empty string.

`string_insert(source, substring: string, index: number): string`
- Inserts a string at index.

`string_grep(s: string, subs: map of strings): string`
- greps a string

`string_split(s, delimiter: string): map`
- Splits a string into multiple parts.

`string_find(s, sub: string): number`
- Finds location of substring in a string. Return -1 if no substring is found.

`string_delete(s: string, index, count: number): string`
- Deletes part of a string at index.

`string_replace(s, old, new: string): string`
- Replaces all `old` with `new` in string s.

`string_replace_ignorecase(s, old, new: string): string`
- Same as string_replace(), but ignore case.

`string_uppercase(s: string): string`
- Returns uppercase string.

`string_lowercase(s: string): string`
- Returns lowercase string.

`string_trim(s: string): string`
- Trims string.

`string_trim_left(s: string): string`
- Trims left of string.

`string_trim_right(s: string): string`
- Trims right of string.

`string_find_regex(s, regex: string): map`
- Returns map of matched string + matched location.
