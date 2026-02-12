-module(test_print).
-compile(export_all).

print_tid(R) when is_reference(R) -> "<ref>";
print_tid(R) -> R.
