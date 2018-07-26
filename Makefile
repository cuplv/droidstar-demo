default: droidstar
	cd client && elm make Main.elm --output index.html
	cp -r server docker-server/_server
	mkdir -p docker-server/_apks
	cp droidstar/driver-app/target/android/output/droidstar-debug.apk docker-server/_apks/CountDownTimer.apk
	docker build docker-server --tag droidstar-demo-server:latest

	mkdir -p docker-client/html
	cp client/index.html docker-client/html/index.html
	cp -r client/css docker-client/html/css
	docker build docker-client --tag droidstar-demo-client:latest

droidstar:
	git clone https://github.com/cuplv/droidstar && cd droidstar && git checkout demo-config && sbt driverApp/android:package

clean:
	rm -rf docker-client/html
	rm -rf docker-server/_server
	rm -rf docker-server/_apks
