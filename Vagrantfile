Vagrant.configure("2") do |config|
  config.vm.box = "ubuntu/focal64"
  config.vm.provision "shell" do |s|
    s.env = {
      "GIT_BRANCH" => ENV["GIT_BRANCH_NAME"]
    }
    s.path = "vagrant-provision.sh"
  end
end
