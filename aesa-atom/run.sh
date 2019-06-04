echo "" > times.out

for i in 1 2 3 4; do
    echo "N=$i"
    echo "N=$i" >> times.out
    time aesa-hl -tp +RTS -ls -N$i >> times.out
    mv -f aesa-hl.eventlog gen/aesa_n$i.eventlog
done
