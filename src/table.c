#include <stdlib.h>
#include <string.h>

#include "memory.h"
#include "object.h"
#include "table.h"
#include "value.h"

void
initTable(Table *table)
{
    table->count = 0;
    table->capacity = 0;
    table->entries = NULL;
}

void
freeTable(Table *table)
{
    FREE_ARRAY(Entry, table->entries, table->capacity);
    initTable(table);
}

// open addressing. NOT separate chaining.
static Entry *
findEntry(Entry *entries, int capacity, ObjString *key)
{
    u32 index = key->hash % capacity;
    // we have tombstones since we don't want to break probe sequence chains (which will make getting to some
    // entries unreachable while deleting). We can also use tombstones to act as entries which can be filled.
    // deleted entries become tombstones instead of being completely deleted since that can break probe sequence
    // chains and make getting to some filled in entries unreachable.
    Entry *tombstone = NULL;

    // This loops through until it gets a completely null entry or has key equal to the one passed in here.
    for (;;)
    {
        Entry *entry = &entries[index];
        // Looking to see if its the tombstone sentinel or a truly null entry.
        if (entry->key == NULL) {
            // the sentinel is a half entry. Key is null, value is true(bool_val).
            if (IS_NIL(entry->value)) {
                // return the truly null entry if no tombstone was encountered, otherwise return the tombstone.
                return tombstone != NULL ? tombstone : entry;
            } else {
                // found a tombstone sentinel entry. set the tombstone to point to the FIRST encountered tombstone.
                // we keep on skipping tombstones ensuring we do not break any probing chains and get to a key
                // present in this table.
                if (tombstone == NULL)
                    tombstone = entry;
            }
        } else if (entry->key == key) {
            // found the key
            return entry;
        }

        index = (index + 1) % capacity;
    }
}

static void
adjustCapacity(Table *table, int capacity)
{
    Entry *entries = ALLOCATE(Entry, capacity);
    for (int i = 0; i < capacity; ++i)
    {
        entries[i].key = NULL;
        entries[i].value = NIL_VAL;
    }

    // We dont want to copy over tombstone entries since we are already building up the probe chains from the
    // ground up.
    table->count = 0;
    // rehash the table. Basically build the hash table up from scratch.
    for (int i = 0; i < table->capacity; ++i)
    {
        Entry *entry = &table->entries[i];
        if (entry->key == NULL) continue;

        // We don't have to copy over tombstone sentinel entries since we are already building the probe sequences
        // here from the ground up. Increment count only if its a non-tombstone entry.
        Entry *destEntry = findEntry(entries, capacity, entry->key);
        destEntry->key = entry->key;
        destEntry->value = entry->value;
        table->count++;
    }

    // Free the old table completely.
    FREE_ARRAY(Entry, table->entries, table->capacity);
    table->entries = entries;
    table->capacity = capacity;
}

bool
tableGet(Table *table, ObjString *key, Value *value)
{
    if (table->count == 0)
        return false;

    Entry *entry = findEntry(table->entries, table->capacity, key);
    if (entry->key == NULL)
        return false;

    *value = entry->value;
    return true;
}

bool
tableSet(Table *table, ObjString *key, Value value)
{
    if ((table->count + 1) > (table->capacity * TABLE_MAX_LOAD)) {
        int capacity = GROW_CAPACITY(table->capacity);
        adjustCapacity(table, capacity);
    }

    // returns the entry with the key(in which its value gets replaced with the value that was sent in here.) that
    // was sent in here or its a null(if this will be a new entry).
    Entry *entry = findEntry(table->entries, table->capacity, key);
    bool isNewKey = entry->key == NULL;

    // NOTE: increment the count of the hashtable only when entry is completely empty and not a tombstone entry.
    // tombstone entries have key as NULL and the value stored in them is true.
    // Tombstones are considered as full entries for the load factor.
    // If the entry returned byu findEntry is a tombstone entry, then we dont change the hashtable count but still use
    // it to store the new key-value pair passed in here.
    if (isNewKey && IS_NIL(entry->value))
        table->count++;

    entry->key = key;
    entry->value = value;
    return isNewKey;
}

void
tableAddAll(Table *from, Table *to)
{
    for (int i = 0; i < from->capacity; ++i)
    {
        Entry *entry = &from->entries[i];
        if (entry->key != NULL)
        {
            tableSet(to, entry->key, entry->value);
        }
    }
}

bool
tableDelete(Table *table, ObjString *key)
{
    if (table->count == 0)
        return false;

    Entry *entry = findEntry(table->entries, table->capacity, key);
    if (entry->key == NULL)
        return false;

    // Place the tombstone in place of the deleted entry, to make sure collision chains are steady.
    entry->key = NULL;
    entry->value = BOOL_VAL(true);
    return true;
}

ObjString *
tableFindString(Table *table, const char *chars, int length, u32 hash)
{
    if (table->count == 0)
        return NULL;

    u32 index = hash % table->capacity;
    for (;;)
    {
        Entry *entry = &table->entries[index];
        if (entry->key == NULL) {
            // Stop if we find an empty non-tombstone entry.
            if (IS_NIL(entry->value))
                return NULL;
        } else if (entry->key->length == length &&
                   entry->key->hash == hash &&
                   memcmp(entry->key->chars, chars, length) == 0)
        {
            // We found the key.
            return entry->key;
        }

        index = (index + 1) % table->capacity;
    }
}

void
markTable(Table *table)
{
    for (int i = 0; i < table->capacity; ++i) {
        Entry *entry = &table->entries[i];
        markObject((Obj *)entry->key);
        markValue(entry->value);
    }
}
