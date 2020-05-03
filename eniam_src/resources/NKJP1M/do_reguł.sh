grep NCOMPOS NKJP1M-corrected-5.07.2017.tab | grep -v brev | grep -v SYMB | \
		# usunięcie niealfabetycznych form
		grep -v "^[^a-zA-ZąęćłńóśźżĄĘĆŁŃÓŚŹŻ]" |
		# analogicznie z lematami
		awk 'match(substr($2, 1, 1), /[a-zA-ZąęćłńóśźżĄĘĆŁŃÓŚŹŻ]/) { print $0 }'
