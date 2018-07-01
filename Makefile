default:
	cd client && elm make Main.elm --output index.html
	cp -r server docker-server/
	git clone https://github.com/cuplv/droidstar && cd droidstar && git checkout demo-config && sbt driverApp/android:package
	cp droidstar/driver-app/target/android/output/droidstar-debug.apk docker-server/
	docker build docker-server
	cp client/index.html docker-client/
	docker build docker-client
