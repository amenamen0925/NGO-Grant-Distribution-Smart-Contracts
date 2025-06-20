const { assertEquals, assertStringIncludes } = require('chai');

describe('NGO Grant Distribution', () => {
  let client;
  let deployer;

  before(async () => {
    client = await Client.createDefaultClient();
    deployer = client.deployer;
  });

  it('should create a new project', async () => {
    const targetAmount = 100000;
    const deadline = 1000;
    const beneficiary = deployer.address;

    const result = await client.createProject(targetAmount, deadline, beneficiary);
    assertEquals(result.success, true);
  });

  it('should allow donations to project', async () => {
    const projectId = 1;
    const amount = 2000;

    const result = await client.donate(projectId, amount);
    assertEquals(result.success, true);
  });

  it('should release funds when goal met', async () => {
    const projectId = 1;
    const result = await client.releaseFunds(projectId);
    assertEquals(result.success, true);
  });

  it('should fail with invalid amount', async () => {
    const projectId = 1;
    const amount = 100;

    const result = await client.donate(projectId, amount);
    assertEquals(result.success, false);
  });
});
