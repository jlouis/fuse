package actions

#Name: string
#Branches: branches: [...string]
#Tags: tags: [...string]

#On: {
	push?:         #Branches
	pull_request?: #Branches
	page_build?:   #Branches
}

#Action: "actions/checkout@v2" | "erlef/setup-beam@v1"
#Uses: {
	uses: #Action
	with?: {
		...
	}
}
#Run: {
	name: string
	run:  string
}
#Steps: #Uses | #Run

#OS_Version: *"ubuntu-latest" | "macos-latest" | "windows_latest"

#Jobs: ci: {
	name:      string
	"runs-on": string
	strategy:
		matrix: {
			otp_vsn: [...string]
			os: [...#OS_Version]
		}
	steps: [...#Steps]
}