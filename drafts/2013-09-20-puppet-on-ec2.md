
# Create instance

Create a new EC2 instance using 32-bit Amazon Linux AMI with instance storage.
Just for this experiment.

# Installation

    sudo yum update

Let's see what's up with Puppet. I'm trying Puppet Enterprise as I haven't
played with it before.

Of course, this means I have to download a tarball for either RHEL 5 or RHEL
6. I'm trying the RHEL 6 version as Amazon Linux was - supposedly - similar to
RHEL 5 with some RHEl 6 when started and is no doubt closer these days.

Hence:

    wget https://s3.amazonaws.com/pe-builds/released/3.0.1/puppet-enterprise-3.0.1-el-6-i386.tar.gz


# SMTP

- Server: email-smtp.us-east-1.amazonaws.com
- Port: 25, 465 or 587
- TLS: Yes
- Username: AKIAJ272HUJNOFHRCK4Q
- Password: AiByvB9+ujIHboT9x73qgnt+g/VXhRJo9o0jYIviJbk5

IAM Username: ses-smtp-user.20130920-210619

# Install

Run the install script and answer the questions. Then wait a while while it
installs all the various bits and pieces and configures them.
