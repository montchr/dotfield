Vagrant.configure("2") do |config|
  config.vm.box = "ubuntu/focal64"
  config.vm.provision "shell" do |s|
    s.env = {
      "CDOM_INIT_HOSTNAME"          => "dotfield",
      "CDOM_INIT_INTERACTIVE"       => "false",
      "CDOM_INIT_NEW_USER_NAME"     => ENV["USER"],
      "CDOM_INIT_NEW_USER_PASSWORD" => "vagrant",
      "CDOM_INIT_TIMEZONE"          => "America/New_York",
      "GIT_BRANCH"                  => ENV["GIT_BRANCH_NAME"],
    }
    s.path = "vagrant-provision.sh"
  end
end
