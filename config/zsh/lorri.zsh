## work with lorri/direnv

# wait for lorri to be ready, then reload direnv
lorri-wait() {
    while tail -n 1 /tmp/lorri-daemon.log | grep -q Started; do
        echo "waiting"
        sleep 2
    done

    direnv reload
}
