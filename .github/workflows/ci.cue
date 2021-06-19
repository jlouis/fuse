let OTP = {
	latest: [24.0]
	all:
}

name: "build"
on: {
	push: branches: [
		"master",
	]
	pull_request: branches: [
		"master",
	]
}
jobs: ci: {
	name:      "Run checks and tests over ${{matrix.otp_vsn}} and ${{matrix.os}}"
	"runs-on": "${{matrix.os}}"
	container: image: "erlang:${{matrix.otp_vsn}}"
	strategy: matrix: {
		otp_vsn: OTP.all
		os: ["ubuntu-latest"]
	}
	steps: [{
		uses: "actions/checkout@v2"
	}, {name: "Build the software"
		run: "make all"
	}, {name: "Run the tests"
		run: "make tests"
	}]
}
