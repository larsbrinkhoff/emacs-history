#ifndef lists_h
#define lists_h

typedef struct List *List;
typedef struct ListItem *ListItem;
typedef char *ListElement;

#define list_item(element) generic_list_item((ListElement) (element))
#define list_element(type, item) ((type) (item == nil ? nil : (item)->element))
#define list_head(list) ((list == nil) ? nil : (list)->head)
#define list_tail(list) ((list == nil) ? nil : (list)->tail)
#define list_next(item) ((item == nil) ? nil : (item)->next)
#define list_prev(item) ((item == nil) ? nil : (item)->prev)
#define list_size(list) (((list) == nil) ? 0 : (list)->nitems)

#define foreach(type, i, list) \
{ \
    register ListItem _item; \
 \
    _item = list_head(list); \
    while (_item != nil) { \
	i = list_element(type, _item); \
	_item = list_next(_item);

#define endfor \
    } \
}

/*
 * Iterate through two equal-sized lists.
 */

#define foreach2(type1, i, list1, type2, j, list2) \
{ \
    register ListItem _item1, _item2; \
 \
    _item1 = list_head(list1); \
    _item2 = list_head(list2); \
    while (_item1 != nil) { \
	i = list_element(type1, _item1); \
	j = list_element(type2, _item2); \
	_item1 = list_next(_item1); \
	_item2 = list_next(_item2);

#define list_islast() (_item == nil)
#define list_curitem(list) (_item == nil ? list_tail(list) : list_prev(_item))

/*
 * Representation should not be used outside except through macros.
 */

struct List {
    Integer nitems;
    ListItem head;
    ListItem tail;
};

struct ListItem {
    ListElement element;
    ListItem next;
    ListItem prev;
};

List list_alloc(/*  */);
ListItem generic_list_item(/* element */);
list_insert(/* item, after, list */);
list_append(/* item, before, list */);
list_delete(/* item, list */);
List list_concat(/* first, second */);
#endif
