#ifndef clox_table_h
#define clox_table_h

#include "common.h"
#include "value.h"

typedef struct {
    ObjString *key;
    Value value;
} Entry;

typedef struct {
    int count;
    int capacity;
    Entry *entries;
} Table;

#define TABLE_MAX_LOAD 0.75

void initTable(Table *table);
void freeTable(Table *table);

/// @brief Get the value for the specified key in the hashtable
/// @param table Hashtable to get the value from.
/// @param key The key for which the value we want to retrieve
/// @param value The out value which sets the value for the given key.
/// @return true if we were able to retrieve the value for the key. false, otherwise.
bool tableGet(Table *table, ObjString *key, Value *value);

/// @brief Add a key-value pair to the hashtable.
/// @param table hashtable to add to.
/// @param key
/// @param value
/// @return true if a new entry is added with the key, false if key was already existing and it overwrote it with
/// the value provided.
bool tableSet(Table *table, ObjString *key, Value value);

/// @brief copy all entries from one table to another.
/// @param from source table to copy from.
/// @param to destination table to copy to.
void tableAddAll(Table *from, Table *to);

/// @brief Delete a key-value pair from the hashtable
/// @param table the hashtable to delete from
/// @param key the key of the key-value pair to remove
/// @return true if the key was found and deleted from the table.
bool tableDelete(Table *table, ObjString *key);

/// @brief Search for a given string in the hashtable.
/// @param table the table to search in
/// @param chars the character array of the string
/// @param length the length of the string
/// @param hash the hash of the string
/// @return pointer to the stored string if it is available.
ObjString *tableFindString(Table *table, const char *chars, int length, u32 hash);

#endif