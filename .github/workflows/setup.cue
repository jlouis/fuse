package actions

name: #Name & "build"
on:   #On & {
	push: branches: [
		_branch,
	]
	pull_request: branches: [
		_branch,
	]
}

jobs: #Jobs
jobs: ci: {
	name:      "Run checks and tests over ${{matrix.otp_vsn}} and ${{matrix.os}}"
	"runs-on": "${{matrix.os}}"
	strategy: matrix: {
		otp_vsn: _versions.otp
		os: ["ubuntu-latest"]
	}
}