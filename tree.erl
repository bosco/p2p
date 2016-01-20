-module(tree).
-export([test/0]).

tree_find_closest(Search_key, {Key, Value, nil, _}) when Search_key < Key ->
	io:format("~p is as close as we get. Value: ~p~n", [Key, Value]);
tree_find_closest(Search_key, {Key, Value, _, nil}) when Search_key >= Key ->
	io:format("~p is as close as we get. Value: ~p~n", [Key, Value]);
tree_find_closest(Search_key, {Key, _, Smaller, _}) when Search_key < Key ->
	tree_find_closest(Search_key, Smaller); 
tree_find_closest(Search_key, {Key, _, _, Bigger}) when Search_key >= Key ->
	tree_find_closest(Search_key, Bigger).

make_tree(0, Tree) ->
	Tree;
make_tree(N, Tree) ->
	make_tree(N - 1, gb_trees:enter(rand:uniform(4096), bogus_value, Tree)).

test() ->
	{_, Tree} = make_tree(1024, gb_trees:empty()),
	tree_find_closest(666, Tree).
