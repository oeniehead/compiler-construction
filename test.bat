@echo off

	echo "Name compiler spl.exe"

	(for %%f in (examples\*.spl) do ( echo "" | spl.exe %%f ) ) > test_output.log
