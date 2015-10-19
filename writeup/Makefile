ts := $(shell /bin/date "+%Y-%m-%d---%H-%M-%S")

plots := ./plots/hours_reporting.pdf ./plots/frac_working.pdf ./plots/scheduling_2014.pdf ./plots/mean_minutes_worked.pdf ./plots/clopening.pdf

$(plots):
	cd ../code/R; ./sched.R

sched.pdf: sched.bib sched.tex $(plots)
	pdflatex -interaction=nonstopmode sched
	bibtex sched
	pdflatex -interaction=nonstopmode sched
	pdflatex -interaction=nonstopmode sched

summary.md: sched.bib sched.tex
	./summary.sh 

backup:
	tar --exclude='../backups' -zcvf ../backups/sched_$(ts).tar.gz ../

snapshot:
	@echo Timestamp is $(ts)
	cp sched.pdf ../snapshots/sched_$(ts).pdf

clean:
	rm *.aux *.log *.out *.bbl *.blg *.dvi *.aux *.fff *.lof *.out 
