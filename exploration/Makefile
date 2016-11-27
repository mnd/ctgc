all : eval run

eval : eval.pl  ../helpers.pl helpers.pl  memory_models.pl  parse.pl  post_process.pl
	gplc eval.pl

run : eval run.sh
	./run.sh
